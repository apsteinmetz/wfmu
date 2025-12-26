library(tidyverse)
library(rvest)
library(httr)
library(xml2)
library(progress)
library(duckplyr)

# ----------------------------------------------
ROOT_URL <- "http://wfmu.org"

ua <- httr::user_agent(
  "wfmu-comment-counter/1.0 (contact: aspteinmetz@yahoo.com)"
)

safe_get_html_raw <- function(url) {
  res <- tryCatch(httr::GET(url, ua, httr::timeout(30)), error = function(e) {
    NULL
  })
  if (is.null(res) || httr::http_error(res)) {
    return(NULL)
  }
  tryCatch(res, error = function(e) NULL)
}

safe_get_html <- function(url) {
  res <- tryCatch(httr::GET(url, ua, httr::timeout(30)), error = function(e) {
    NULL
  })
  if (is.null(res) || httr::http_error(res)) {
    return(NULL)
  }
  tryCatch(read_html(res), error = function(e) NULL)
}

altArtistNames <- c(
  'THE STOOGE',
  'Band',
  'Singer',
  'Artist',
  'artist',
  'ARTIST'
)
altTitleNames <- c('THE SONG', 'Track', 'Song', 'Title', 'TITLE', 'title')

djKey <- read_parquet_duckdb("data/djKey.parquet")
playlistURLs <- read_parquet_duckdb("data/playlistURLs.parquet") |>
  rename(AirDate = date)
playlists_raw <- read_parquet_duckdb("data/playlists_raw.parquet") |>
  as_tibble()

existing_shows <- playlists_raw |>
  select(DJ, AirDate) |>
  distinct()

missing_shows <- playlistURLs |>
  anti_join(existing_shows, by = c("DJ", "AirDate")) |>
  as_tibble() |>
  mutate(show_url = map_chr(show_id, expand_show_id)) |>
  # get rid of 1-2 letter show_ids that are pointers to fill-ins
  filter(!str_detect(show_url, "^[A-Za-z]{2,3}$"))


# case where hypen is the separator within one table column
BK_case <- function(table_2) {
  playlist <- table_2 |>
    str_split("\n\n") |>
    unlist() |>
    as_tibble() |>
    filter(value != "") |>
    separate(
      value,
      into = c("Artist", "Title"),
      sep = "\n-\n",
      extra = "merge"
    ) |>
    mutate(
      Artist = str_squish(Artist),
      Title = str_replace_all(str_squish(Title), '\\"', " ")
    )

  return(playlist)
}


# ...existing code...

extract_playlist <- function(show_info) {
  html_doc <- safe_get_html(show_info$show_url)
  if (is.null(html_doc)) {
    result <- tibble(
      DJ = show_info$DJ,
      AirDate = show_info$AirDate,
      Artist = character(),
      Title = character()
    )
    return(result)
  }
  # preprocess. If the page has frames get the first frame
  frames <- html_doc |> html_elements("frame")
  if (length(frames) > 0) {
    frame_src <- frames |> html_attr("src") |> first()

    if (!is.na(frame_src) && frame_src != "") {
      # construct full URL if needed
      if (!str_detect(frame_src, "^http")) {
        frame_src <- paste0(ROOT_URL, frame_src)
      }
      html_doc <- safe_get_html(frame_src)
      if (is.null(html_doc)) {
        result <- tibble(
          DJ = show_info$DJ,
          AirDate = show_info$AirDate,
          Artist = character(),
          Title = character()
        )
        return(result)
      }
    }
  }

  # First try: Look for td elements with specific classes
  artist_nodes <- html_doc |> html_elements("td.col_artist")
  title_nodes <- html_doc |> html_elements("td.col_song_title")

  if (length(artist_nodes) > 0 && length(artist_nodes) == length(title_nodes)) {
    # Extract only the direct text content, not from child elements
    artists <- artist_nodes |>
      map_chr(
        ~ {
          # Get all text nodes and take the first non-empty one
          text_content <- xml_text(.x, trim = TRUE)
          str_trim(str_split_i(text_content, "→", 1))
        }
      )

    titles <- title_nodes |>
      map_chr(
        ~ {
          # Get all text nodes and take the first non-empty one
          text_content <- xml_text(.x, trim = TRUE)
          str_trim(str_split_i(text_content, "→", 1))
        }
      )

    result <- tibble(
      Artist = artists,
      Title = titles
    ) |>
      filter(!is.na(Artist) & !is.na(Title) & Artist != "" & Title != "")

    if (nrow(result) > 0) {
      result <- result |>
        mutate(
          .before = "Artist",
          DJ = show_info$DJ,
          AirDate = show_info$AirDate
        )
      return(result)
    }
  }

  # ...existing code...

  # second try: Look for tables in the HTML that have "Artist" and "Title" headers
  tables <- html_doc |>
    html_elements("table") |>
    map(html_table)

  if (length(tables) > 0) {
    # Try each table until we find one with playlist data
    for (tbl in tables) {
      if (ncol(tbl) == 0 || nrow(tbl) < 2) {
        next
      }
      # Convert to tibble and fix column names
      tbl <- as_tibble(tbl, .name_repair = "unique")

      # Get column names (may be in first row if no header)
      col_names <- names(tbl)
      first_row <- if (nrow(tbl) > 0) as.character(tbl[1, ]) else character()

      # Check for artist column (case-insensitive)
      # Only match if the column name/first row value looks like a real header
      artist_col <- NULL
      for (name in altArtistNames) {
        # Skip auto-generated column names like X1, X2, etc.
        valid_cols <- which(!str_detect(col_names, "^X\\d+$"))
        idx <- which(
          (str_detect(
            col_names,
            regex(paste0("^", name, "$"), ignore_case = TRUE)
          ) |
            str_detect(
              first_row,
              regex(paste0("^", name, "$"), ignore_case = TRUE)
            )) &
            seq_along(col_names) %in% valid_cols
        )
        if (length(idx) > 0) {
          artist_col <- idx[1]
          break
        }
      }

      # Check for title column (case-insensitive)
      title_col <- NULL
      for (name in altTitleNames) {
        # Skip auto-generated column names like X1, X2, etc.
        valid_cols <- which(!str_detect(col_names, "^X\\d+$"))
        idx <- which(
          (str_detect(
            col_names,
            regex(paste0("^", name, "$"), ignore_case = TRUE)
          ) |
            str_detect(
              first_row,
              regex(paste0("^", name, "$"), ignore_case = TRUE)
            )) &
            seq_along(col_names) %in% valid_cols
        )
        if (length(idx) > 0) {
          title_col <- idx[1]
          break
        }
      }

      # If we found both columns
      if (!is.null(artist_col) && !is.null(title_col)) {
        # Check if first row is header
        start_row <- if (
          any(str_detect(
            first_row[artist_col],
            regex(
              paste(c(altArtistNames, "ARTIST", "artist"), collapse = "|"),
              ignore_case = TRUE
            )
          ))
        ) {
          2
        } else {
          1
        }

        result <- tbl |>
          slice(start_row:n()) |>
          select(Artist = all_of(artist_col), Title = all_of(title_col)) |>
          mutate(
            Artist = str_trim(str_split_i(as.character(Artist), "\\n", 1)),
            Title = str_trim(str_split_i(as.character(Title), "\\n", 1))
          ) |>
          filter(!is.na(Artist) & !is.na(Title) & Artist != "" & Title != "")

        if (nrow(result) > 0) {
          result <- result |>
            mutate(
              .before = "Artist",
              DJ = show_info$DJ,
              AirDate = show_info$AirDate
            )
          return(result)
        }
      }

      # ...existing code...
    }
  }

  # ...existing code...
  # third try: look for tables with at least two columns and assume first two columns are Artist and Title
  if (length(tables) > 0) {
    # limit to first two tables
    tables <- tables[1:min(2, length(tables))]

    for (tbl in tables) {
      if (ncol(tbl) < 2 || nrow(tbl) < 2) {
        next
      }

      # Convert to tibble and fix column names
      tbl <- as_tibble(tbl, .name_repair = "unique")

      # Try assuming first two columns are Artist and Title
      result <- tbl |>
        select(Artist = 1, Title = 2) |>
        mutate(
          Artist = str_trim(str_split_i(as.character(Artist), "\\n", 1)),
          Title = str_trim(str_split_i(as.character(Title), "\\n", 1))
        ) |>
        filter(!is.na(Artist) & !is.na(Title) & Artist != "" & Title != "")

      # Check if this looks like valid playlist data
      # (at least 3 rows and not too many identical values)
      if (nrow(result) >= 3) {
        # Check that we don't have the same artist/title repeated for all rows
        unique_artists <- n_distinct(result$Artist)
        unique_titles <- n_distinct(result$Title)

        if (unique_artists > 1 || unique_titles > 1) {
          result <- result |>
            mutate(
              .before = "Artist",
              DJ = show_info$DJ,
              AirDate = show_info$AirDate
            )
          return(result)
        }
      }
    }
  }

  # fourth try: Look for td.song elements (combined artist - title format)
  song_nodes <- html_doc |> html_elements("td.song")

  if (length(song_nodes) > 0) {
    print(paste("Found td.song elements for", show_info$DJ, show_info$AirDate))
    songs <- song_nodes |>
      map_chr(
        ~ {
          text_content <- xml_text(.x, trim = TRUE)
          # Take only the first line
          str_trim(str_split_i(text_content, "\\n", 1))
        }
      )

    # Filter out empty entries and those without the separator
    songs_clean <- songs[songs != "" & str_detect(songs, " - ")]

    if (length(songs_clean) > 0) {
      result <- tibble(combined = songs_clean) |>
        separate(
          combined,
          into = c("Title", "Artist"),
          sep = " - ",
          extra = "merge"
        ) |>
        mutate(
          Artist = str_trim(Artist),
          Title = str_trim(Title)
        ) |>
        filter(!is.na(Artist) & !is.na(Title) & Artist != "" & Title != "")

      if (nrow(result) > 0) {
        result <- result |>
          select(Artist, Title) |>
          mutate(
            .before = "Artist",
            DJ = show_info$DJ,
            AirDate = show_info$AirDate
          )
        return(result)
      }
    }
  }

  # fifth try: BK_case for table layouts where second table has playlist and artist - title is one table column
  #tables <- html_doc |>
  #  html_elements("table") |>
  #  map(html_table)
  # print(show_info$AirDate)
  if (length(tables) >= 2) {
    table_2 <- tables[[2]][2]
    # when no playlist sometimes the second table is  listener comments
    if (nrow(table_2) > 0 & !str_detect(table_2[1, 1], "Listener")) {
      result <- BK_case(tables[[2]][2])
      if (nrow(result) > 0) {
        result <- result |>
          select(Artist, Title) |>
          mutate(
            .before = "Artist",
            DJ = show_info$DJ,
            AirDate = show_info$AirDate
          )
        return(result)
      }
    }
  }
  # sixth try. there are no tables at all. look for  lines with a colon, bar  or quotes separating artist and title
  # e.g dj DK, BT
  text_content <-
    html_doc |> html_text()
  lines <- str_split(text_content, "\n") |> unlist() |> str_trim()
  # keep only lines with a colon, starting and ending with non-space characters
  playlist_lines <- lines[str_detect(lines, "^[A-Za-z ]+ : [A-Za-z ]+")]
  if (length(playlist_lines) == 0) {
    #keep lines with an unquoted string followed by a quoted string
    playlist_lines <- lines[str_detect(lines, '.+ \\".+\\"')]
  }
  if (length(playlist_lines) == 0) {
    #keep lines with a bar followed by a bar
    playlist_lines <- lines[str_detect(lines, '.+ \\|.+\\|')]
  }
  if (length(playlist_lines) > 0) {
    result <- tibble(line = playlist_lines) |>
      separate(
        line,
        into = c("Artist", "Title"),
        sep = ':|\\"|\\|',
        extra = "drop"
      ) |>
      mutate(
        Artist = str_trim(Artist),
        Title = str_trim(Title)
      ) |>
      filter(!is.na(Artist) & !is.na(Title) & Artist != "" & Title != "")

    if (nrow(result) > 0) {
      result <- result |>
        select(Artist, Title) |>
        mutate(
          .before = "Artist",
          DJ = show_info$DJ,
          AirDate = show_info$AirDate
        )
      return(result)
    }
  }

  # Return empty tibble if no valid playlist found
  tibble(
    DJ = show_info$DJ,
    AirDate = show_info$AirDate,
    Artist = "",
    Title = ""
  )
}

# ...existing code...
test_pages <- missing_shows |>
  slice_sample(n = 100)

all_playlists <- test_pages |>
  pmap(function(DJ, AirDate, show_id, show_url) {
    extract_playlist(tibble(
      DJ = DJ,
      AirDate = AirDate,
      show_id = show_id,
      show_url = show_url
    ))
  })

# Find indices of playlists with only one row
single_row_indices <- all_playlists |>
  map_int(nrow) |>
  (\(x) which(x == 1))()

# Filter test_pages to just those rows
problem_pages <- test_pages |>
  slice(single_row_indices)

problem_pages
all_playlists <- problem_pages |>
  pmap(function(DJ, AirDate, show_id, show_url) {
    extract_playlist(tibble(
      DJ = DJ,
      AirDate = AirDate,
      show_id = show_id,
      show_url = show_url
    ))
  })

for (n in 1:nrow(problem_pages)) {
  problem_pages[n, 4] |>
    pull(show_url) |>
    # open external browser to inspect
    browseURL()
}
