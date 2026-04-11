# scrape DJs

library(tidyverse)
library(rvest)
library(httr)
library(stringr)
library(xml2)
library(duckplyr)
library(rlang)

# ==================================================
# setup
source("func_get_show_links.R")
source("func_get_show_names.R")

base_url = "https://www.wfmu.org/playlists"
date_regex <- "\\b(?:January|February|March|April|May|June|July|August|September|October|November|December|Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec)\\.?\\s+\\d{1,2},\\s+\\d{4}\\b"
pause = 0.5
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

safe_get_html_raw <- function(url) {
  res <- tryCatch(httr::GET(url, ua, httr::timeout(30)), error = function(e) {
    NULL
  })
  if (is.null(res) || httr::http_error(res)) {
    return(NULL)
  }
  res_text <- res |>
    content(as = "text", encoding = "UTF-8") |>
    strsplit("<br>") |>
    unlist()
  tryCatch(res_text, error = function(e) NULL)
}
# helper: absolute urls
abs <- function(href, base) {
  href <- href[!is.na(href)]
  if (length(href) == 0) {
    return(character(0))
  }
  xml2::url_absolute(href, base)
}
#-------------- MAIN -----------------
# spoken word shows
# This also excludes DJs where we couldn't extract valid playlist URLs.
excludeDJs <-
  sort(c(
    'AQ',
    'JM',
    'IP',
    'AC',
    'SD',
    'DX',
    'WP',
    'SD',
    'AF',
    'HA',
    'BC',
    'CP',
    'HP',
    'JP',
    'GM',
    'DC',
    'CC',
    'DU',
    'ES',
    'LW',
    'IM',
    'LL',
    'LU',
    'NW',
    'GJ',
    'NP',
    'UP',
    'ZZ',
    'R6',
    'FC',
    'SY',
    'TI',
    'LK',
    'RC',
    'TD',
    'TH',
    'PZ',
    'NC',
    'AZ',
    'PW',
    'WV',
    'SE',
    'AP',
    'B3',
    'WC'
  ))

all_show_names <- get_show_names()

# useful info for inspection
excluded_shows <- all_show_names %>%
  filter(DJ %in% excludeDJs) |>
  arrange(DJ)

show_names <- all_show_names %>%
  filter(!(DJ %in% excludeDJs)) |>
  arrange(DJ)

# refreshed show URLs for all DJs, or just update existing ones?
NO_REFRESH <- FALSE
# do we need to scrape prior years or just update existing shows?
UPDATE_ONLY <- TRUE

# get all show URLs for all music DJs
# if show_urls.rds exists, load it instead of re-fetching
# set NO_REFRESH globally
if (file.exists("data/wfmu_show_urls.rds") & NO_REFRESH) {
  show_urls <- readRDS("data/wfmu_show_urls.rds")
  dj_profiles <- readRDS("data/dj_profiles.rds")
} else {
  if (UPDATE_ONLY) {
    show_urls <- readRDS("data/wfmu_show_urls.rds")
    show_urls_new <- show_names$DJ |>
      map_dfr(\(x) get_show_links(x, update_only = TRUE))
    show_urls <- bind_rows(show_urls_new, show_urls) |>
      distinct()
    dj_profiles <- readRDS("data/dj_profiles.rds") |>
      bind_rows(dj_profiles) |>
      distinct()
  } else {
    print( "Scraping show URLs for all DJs. This may take a while...")
    show_urls <- show_names$DJ |>
      map_dfr(\(x) get_show_links(x, update_only = FALSE)) |>
      distinct()
  }
}
show_urls <- show_urls |>
  filter(!(DJ %in% excludeDJs))
saveRDS(show_urls, "data/wfmu_show_urls.rds")
saveRDS(dj_profiles, "data/dj_profiles.rds")
compute_parquet(show_urls, "data/playlistURLs.parquet")


showCount <- show_urls %>%
  group_by(DJ) %>%
  summarise(showCount = n()) %>%
  arrange(desc(showCount))

# get first show and last show dates for each DJ
showDates <- show_urls %>%
  summarise(
    .by = "DJ",
    FirstShow = min(AirDate, na.rm = TRUE),
    LastShow = max(AirDate, na.rm = TRUE)
  ) %>%
  arrange(DJ)

numWords <- 2
show_names <- show_names %>%
  mutate(
    ShowToken = str_squish(ShowName), # collapse multiple spaces
    ShowToken = str_to_title(ShowToken), # title case
    ShowToken = stringr::word(ShowToken, 1, pmin(numWords, str_count(ShowToken, "\\S+")), sep = " ") # first two words
  )

# # testing
# show_name_tokens <- djkey %>%
#   mutate(
#     ShowToken = str_squish(ShowName), # collapse multiple spaces
#     ShowToken = tolower(ShowToken), # lower case
#     ShowToken = gsub("and | of | the ", " ", ShowToken),
#     ShowToken = str_to_title(ShowToken), # title case
#     ShowToken = gsub("[^A-Za-z0-9 ]", "", ShowToken),
#     ShowToken = gsub(paste0("^\\s*(\\S+(?:\\s+\\S+){0,", numWords - 1, "}).*$"),
#     "\\1",
#     ShowToken
#   )) |>
#   select(DJ, ShowToken)

# #testing
# djKey <- djKey |>
#   select(-ShowToken) |>
#   left_join(show_name_tokens, by = "DJ")

djKey <- dj_profiles |>
  left_join(show_names, by = "DJ") |>
  left_join(showCount, by = "DJ") |> 
  left_join(showDates, by = "DJ")  |>
  # remove ShowName string from other_shownames
  mutate(
    other_shownames = str_trim(str_remove(other_shownames, ShowName))
  ) |>
  mutate(other_shownames = str_remove(other_shownames, "'s show")) |>
  mutate(other_shownames = str_remove(other_shownames, "^\\\n")) |>
  # replace empty string with "none"
  mutate(
    other_shownames = ifelse(other_shownames == "", "none", other_shownames)
  ) |>
  unique() |>
  # drop_na() |>
  as_tibble() |>
  select(
    DJ,
    ShowName,
    onSched,
    Channel,
    other_shownames,
    showCount,
    FirstShow,
    LastShow,
    profileURL,
    ShowToken
  )

#limit analysis to DJs with at least numShows shows.
# numShows <- 10
# djKey <- djKey %>%
#  filter(showCount > numShows)

compute_parquet(djKey, "data/djKey.parquet")
# save playlistURLs as parquet
compute_parquet(show_urls, "data/playlistURLs.parquet")
