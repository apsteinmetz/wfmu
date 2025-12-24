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
  'Artist'
)
altTitleNames <- c('THE SONG', 'Track', 'Song', 'Title')

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

# ...existing code...

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

  # Fallback to table extraction methods
  tables <- html_doc |>
    html_elements("table") |>
    map(html_table, fill = TRUE)

  if (length(tables) == 0) {
    return(tibble(
      DJ = show_info$DJ,
      AirDate = show_info$AirDate,
      Artist = character(),
      Title = character()
    ))
  }

  # Try each table until we find one with playlist data
  for (tbl in tables) {
    if (ncol(tbl) == 0 || nrow(tbl) == 0) {
      next
    }

    # Convert to tibble and fix column names
    tbl <- as_tibble(tbl, .name_repair = "unique")

    # Get column names (may be in first row if no header)
    col_names <- names(tbl)
    first_row <- if (nrow(tbl) > 0) as.character(tbl[1, ]) else character()

    # Check for artist column
    artist_col <- NULL
    for (name in c(altArtistNames, "ARTIST", "artist")) {
      idx <- which(col_names == name | first_row == name)
      if (length(idx) > 0) {
        artist_col <- idx[1]
        break
      }
    }

    # Check for title column
    title_col <- NULL
    for (name in c(altTitleNames, "TITLE", "title")) {
      idx <- which(col_names == name | first_row == name)
      if (length(idx) > 0) {
        title_col <- idx[1]
        break
      }
    }

    # If we found both columns
    if (!is.null(artist_col) && !is.null(title_col)) {
      # Check if first row is header
      start_row <- if (
        first_row[artist_col] %in% c(altArtistNames, "ARTIST", "artist")
      ) {
        2
      } else {
        1
      }

      result <- tbl |>
        slice(start_row:n()) |>
        select(Artist = all_of(artist_col), Title = all_of(title_col)) |>
        mutate(
          Artist = str_trim(str_split_i(Artist, "\\n", 1)),
          Title = str_trim(str_split_i(Title, "\\n", 1))
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

    # Check for combined artist-title column with " - " separator
    for (i in seq_along(col_names)) {
      sample_values <- tbl[[i]][1:min(5, nrow(tbl))]
      non_na_values <- sample_values[!is.na(sample_values)]
      if (
        length(non_na_values) > 0 && sum(str_detect(non_na_values, " - ")) >= 2
      ) {
        result <- tbl |>
          select(combined = all_of(i)) |>
          filter(!is.na(combined) & str_detect(combined, " - ")) |>
          mutate(combined = str_trim(str_split_i(combined, "\\n", 1))) |>
          separate(
            combined,
            into = c("Artist", "Title"),
            sep = " - ",
            extra = "merge"
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
    }

    # Check for pattern where title is in quotes: Artist "Title"
    for (i in seq_along(col_names)) {
      sample_values <- tbl[[i]][1:min(5, nrow(tbl))]
      non_na_values <- sample_values[!is.na(sample_values)]
      # Look for pattern with quoted titles
      if (
        length(non_na_values) > 0 &&
          sum(str_detect(non_na_values, '"[^"]+"')) >= 2
      ) {
        result <- tbl |>
          select(combined = all_of(i)) |>
          filter(!is.na(combined) & str_detect(combined, '"[^"]+"')) |>
          mutate(combined = str_trim(str_split_i(combined, "\\n", 1))) |>
          mutate(
            Title = str_extract(combined, '"([^"]+)"', group = 1),
            Artist = str_trim(str_remove(combined, '"[^"]+"'))
          ) |>
          select(Artist, Title) |>
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
    }
  }

  # Return empty tibble if no valid playlist found
  tibble(
    DJ = show_info$DJ,
    AirDate = show_info$AirDate,
    Artist = character(),
    Title = character()
  )
}

#   slice_sample(n = 100)

methods_restore()
all_playlists <- test_pages |>
  pmap(function(DJ, AirDate, show_id, show_url) {
    extract_playlist(tibble(
      DJ = DJ,
      AirDate = AirDate,
      show_id = show_id,
      show_url = show_url
    ))
  }) #|>
bind_rows()
