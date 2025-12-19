# scrape playlists

library(tidyverse)
library(rvest)
library(xml2)
library(progress)
library(duckplyr)

# ----------------------------------------------
ROOT_URL <- "http://wfmu.org"

# --------------------------------------------------------------------------
altArtistNames <- c(
  'THE STOOGE',
  'Band',
  'Singer',
  'Artist'
)
altTitleNames <- c('THE SONG', 'Track', 'Song', 'Title')
altHeaderNames <- c(altArtistNames, altTitleNames)
MAXLEN = 60L #how long should we let artist and title names be. Truncate longer
header_th_xpath <- paste(
  "//th='Track'",
  "or //th='Title'",
  "or //th='Song'",
  "or //th='THE SONG'"
)
header_td_xpath <- paste(
  "//td='Track'",
  "or //td='Title'",
  "or //td='Song'",
  "or //td='THE SONG'",
  "or //td='Artist'",
  "or //td='THE SONG'",
  "or //td='THE STOOGE'"
)


#------------------------------------------------------------------
try_BK <- function(wp) {
  #assume first field is title and second is artist separated by dash
  #doing it it two steps assure same number of artists and titles
  title_artist <- wp |>
    html_nodes(xpath = "//table[2]") |>
    html_text() |>
    str_extract_all("\n[\\S ]+\n-\n[\\S ]+\n") |>
    pluck(1)

  Title <- title_artist |>
    str_extract_all("\n[\\S ]+\n-") |>
    str_replace_all("\n(-)?", "")

  Artist <- title_artist |>
    str_extract_all("\n-\n[\\S ]+\n") |>
    str_replace_all("\n(-)?", "")
  plraw <- tibble(Artist, Title)
}
#------------------------------------------------------------------
try_HN <- function(wp) {
  #assume first field is title and second is artist separated by colon
  #doing it it two steps assure same number of artists and titles
  title_artist <- wp |>
    html_nodes(xpath = "//table[2]") |>
    html_text() |>
    str_extract_all("\n[\\S ]+: [\\S ]+\n") |>
    pluck(1)

  Artist <- title_artist |>
    str_extract_all("\n[\\S ]+: ") |>
    str_replace_all("(\n)|(: )", "")

  Title <- title_artist |>
    str_extract_all(": [\\S ]+\n") |>
    str_replace_all("(\n)|(: )", "")
  plraw <- tibble(Artist, Title)
} #------------------------------------------------------------------

fixHeaders <- function(pl) {
  #takes a data frame
  nm <- which(names(pl) %in% altArtistNames)
  tt <- which(names(pl) %in% altTitleNames)
  if (length(nm) > 0) {
    names(pl)[nm] <- "Artist"
  } else {
    pl = NULL
    return(pl)
  }
  if (length(tt) > 0) {
    names(pl)[tt] <- "Title"
  } else {
    pl$Title = NA
  }

  return(pl)
}

#--------------------------------------------------------------------
get_playlist <- function(show_info) {
  # turn off duckplyr
  # methods_restore()
  dj <- show_info$DJ
  airDate <- show_info$date
  show_url <- paste0(ROOT_URL, "/playlists/", show_info$show_id)

  wholepage <- tryCatch(
    read_html(show_url),
    error = function(e) {
      NA
    }
  ) #handle 404 errors
  if (is.na(wholepage)) {
    return(tibble(DJ = "", AirDate = as.Date(NA), Artist = "", Title = ""))
  }
  plraw <- NULL
  #hand-rolled
  #simplest case. A table with obvious header names
  if (!is.na(wholepage |> html_node(xpath = "//th[@class='song']"))) {
    table_shell <- xml_new_root("table")
    #remove single column rows, I hope nothing else.
    wholepage |>
      html_nodes(xpath = "//td[@colspan='8']") |>
      xml_remove(free = T)
    plraw <- wholepage |>
      html_nodes(xpa = "//tr[td[@class ='song']] | //tr[th[@class ='song']]")
    for (node in plraw) {
      xml_add_child(table_shell, node)
    }
    plraw <- table_shell |>
      html_node(xpath = "//table") |>
      html_table(fill = TRUE)
  } else {
    # no 'th' but are there rows in a table with td of class=song?  get the table
    if (!is.na(wholepage |> html_node(xpath = "//td[@class='song']"))) {
      plraw <- wholepage |>
        html_node(xpath = "//td[@class='song']/ancestor::table") |>
        html_table(fill = T)
      #now find the row that has the header
      for (n in 1:nrow(plraw)) {
        if (TRUE %in% (plraw[n, ] %in% altHeaderNames)) {
          names(plraw) <- plraw[n, ]
          plraw <- plraw[n + 1:nrow(plraw), ]
          break
        }
        if (n == nrow(plraw)) {
          plraw <- NULL
        }
      }
    }
  }

  if (is.null(plraw)) {
    # no song class, now what? is it a table? try to  find header
    #seems like cellspacing means its a row column thing
    pl_table <- wholepage |>
      html_node(xpath = "//table[@cellspacing and @cellpadding]")
    num_rows <- pl_table |> html_nodes("tr") |> length()
    if (num_rows > 2) {
      pl_table <- html_table(pl_table, fill = TRUE)
      if (any(names(pl_table) %in% altHeaderNames)) {
        plraw <- pl_table
      } else {
        # try one more ``
        #scan until we find the playlist header
        for (n in 1:nrow(pl_table)) {
          if (TRUE %in% (pl_table[n, ] %in% altHeaderNames)) {
            names(pl_table) <- pl_table[n, ]
            plraw <- pl_table[n + 1:nrow(pl_table), ]
            break
          }
        }
      }
    }
  }

  # SPECIAL DJ TREATMENT
  if (is.null(plraw)) {
    #try idiosyncratic djs
    if (dj == "TW") {
      plraw <- try_BK(wholepage)
    }
    if (dj == "HN") plraw <- try_HN(wholepage)
  }

  # new 2020 style headers.  Nobody told me about it!
  if (is.null(plraw)) {
    artists <- wholepage |>
      html_nodes(xpath = "//td[@class='song col_artist']") |>
      html_text() |>
      str_remove_all("\\n") |>
      str_trim()
    titles <- wholepage |>
      html_nodes(xpath = "//td[@class='song col_song_title']") |>
      html_elements("font") |>
      html_text() |>
      str_remove_all("\\n")

    plraw <- tibble(Artist = artists, Title = titles)
    if (nrow(plraw) == 0) plraw <- NULL
  }

  plraw <- fixHeaders(plraw)
  # final clean up if we have something
  if (is.null(plraw)) {
    playlist <- NULL
    print(paste("DUD", show_url))
  } else {
    if (TRUE %in% is.na(names(plraw))) {
      plraw <- plraw[, -which(is.na(names(plraw)))]
    } #sometimes an NA column

    playlist <- plraw |>
      select(Artist, Title) |>
      na.omit() |>
      mutate(DJ = dj, AirDate = airDate, Artist = substr(Artist, 1L, MAXLEN)) |>
      mutate(Artist = str_to_title(Artist)) |> # not a duckplyr function
      mutate(Artist = gsub("[\r\n].*$", "", Artist)) |>
      mutate(Artist = gsub("\\(.*$", "", Artist)) |>
      mutate(Artist = gsub("^\\s+|\\s+$", "", Artist)) |>
      mutate(Title = substr(Title, 1L, MAXLEN)) |>
      mutate(Title = str_to_title(Title)) |> # not a duckplyr function
      mutate(Title = gsub("[\r\n].*$", "", Title)) |>
      mutate(Title = gsub("\\(.*$", "", Title)) |>
      mutate(Title = gsub("^\\s+|\\s+$", "", Title)) |>
      filter(Artist != '') |>
      filter(!is.na(Artist))
    # just to track progress
    if (is.null(playlist)) {
      print(paste("No Playlist", show_url))
    } else {
      print(playlist[1:5, ])
    }
  }
  # methods_overwrite()
  return(playlist)
}
#-------------- MAIN -----------------
djKey <- read_parquet_duckdb("data/djKey.parquet")
playlistURLs <- read_parquet_duckdb("data/playlistURLs.parquet")
playlists_raw <- read_parquet_duckdb("data/playlists_raw.parquet") |>
  as_tibble()

#careful not to trash intermediate results!
UPDATE_ONLY = TRUE
if (UPDATE_ONLY) {
  existing_shows <- playlists_raw |>
    select(DJ, AirDate) |>
    distinct()

  missing_shows <- playlistURLs |>
    anti_join(existing_shows, by = "DJ")

  playlists_temp <- tibble(
    DJ = character(),
    AirDate = as.Date(character()),
    Artist = character(),
    Title = character()
  )
  # progress bar for scraping missing shows
  #pb <- progress::progress_bar$new(
  #  total = nrow(missing_shows),
  #  format = "  Scraping [:bar] :current/:total (:percent) - :eta left - :message",
  #  clear = FALSE
  #)
  methods_restore()
  for (n in 1:nrow(missing_shows)) {
    # advance bar and set message for current DJ
    # pb$tick(tokens = list(message = paste0(missing_shows[n,]$DJ, " ", missing_shows[n,]$show_id)))

    dj <- missing_shows[n, ]$DJ
    show_id <- missing_shows[n, ]$show_id
    print(paste(n, dj, show_id, Sys.time()))
    playlist <- get_playlist(missing_shows[n, ])
    if (!is.null(playlist)) {
      playlists_temp <- bind_rows(playlists_temp, playlist)
    }

    #    if (is.null(playlist)) {
    #  pb$terminate()
    #     break # done with this DJ
    #   }
  }
  #save to disk after each dj
  # compute_parquet(playlists_temp, "data/playlists_temp.parquet")
}

bad_Tables <- anti_join(tibble(DJ = djKey$DJ), playlists_temp) |>
  left_join(djKey)

save(bad_Tables, file = "data/bad_tables.rdata")


playlists_temp <- playlists_temp |>
  filter(Artist != Title) |> #single column span across table.  Not a song.
  distinct()

# playlists_temp <- read_parquet_duckdb("data/playlists_temp.parquet")
playlists_raw <- bind_rows(playlists_raw, playlists_temp) |>
  distinct()

compute_parquet(playlists_raw, "data/playlists_raw.parquet")
