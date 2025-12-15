# scrape DJs

library(rvest)
library(httr)
library(stringr)
library(xml2)
library(tidyverse)
library(progress)
library(duckplyr)
library(rlang)

# ==================================================
# setup
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
source("func_get_show_links.R")
source("func_get_show_names.R")
# ==================================================

# fetch base and find DJ archive links like /playlists/<dj>
base_doc <- safe_get_html(base_url)
if (is.null(base_doc)) {
  abort("Failed to fetch base URL")
}

all_hrefs <- base_doc %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  discard(is.na) %>%
  unique()

dj_ids <- all_hrefs[str_detect(all_hrefs, "^/playlists/[^/]+$")] |>
  str_extract("(?<=/playlists/)[^/]+$") |>
  unique() |>
  # remove "JM" from list, never comments
  discard(\(x) x == "JM")


# get all show URLs for all DJs
NO_REFRESH <- TRUE
# if show_urls.rds exists, load it instead of re-fetching
# set NO_REFRESH globally
if (file.exists("data/wfmu_show_urls.rds") & NO_REFRESH) {
  show_urls <- readRDS("data/wfmu_show_urls.rds")
} else {
  show_urls <- dj_ids |>
    map_dfr(get_show_links) |>
    distinct()
  saveRDS(show_urls, "data/wfmu_show_urls.rds")
  saveRDS(dj_profiles, "data/new_djKey.rds")
}

# work stopping point here
#-------------------------------------------
getDJURLs <- function() {
  DJURLs <- abs(paste0("/playlists/", dj_ids), base_url)
  return(DJURLs)
}
#-------------------------------------------
DJURLs <- getDJURLs()

#---------------------------------------------------
# get the shownames for a DJ
getShowNames <- function(DJURLs) {
  pb <- progress_bar$new(
    format = "  Getting Show :what [:bar] :percent eta: :eta",
    clear = FALSE,
    total = length(DJURLs)
  )
  djKey_raw <- data.frame()
  for (page in DJURLs) {
    singleDJ <- read_html(page)
    showName <- html_node(singleDJ, "title") %>% html_text()
    showName <- gsub("\n", "", sub("Playlists and Archives for ", "", showName))
    showName <- str_replace(showName, 'WFMU:', "")
    showName <- str_replace_all(showName, ':Playlists and Archives', "")
    DJ <- sub("http://wfmu.org/playlists/", "", page)
    pb$tick(tokens = list(what = DJ))
    profileURL <- singleDJ %>%
      html_nodes(xpath = "//a[contains(@href,'profile')]") %>%
      html_attr("href") |>
      pluck(1)
    # if profile URL is not found, use the DJ URL
    if (length(profileURL) == 0) {
      profileURL <- page
      other_shownames <- "none"
    } else {
      other_shownames <- get_other_shownames(profileURL, showName)
      if (length(other_shownames) == 0) other_shownames <- "none"
    }
    # print(DJ)
    djKey_raw <- rbind(
      djKey_raw,
      data.frame(
        DJ = DJ,
        ShowName = showName,
        profileURL = profileURL,
        other_shows = other_shownames
      )
    )
  }
  # now identifty those DJs which are currently ON MIC
  djKey_raw$onSched <- 'YES'
  djKey_raw$onSched[which(djKey_raw$DJ %in% getDJsOffSched())] <- 'NO'
  #strip "WFMU" and "Playlists and Archives" and some punctuation
  djKey_raw$ShowName <- str_replace_all(
    djKey_raw$ShowName,
    "(P|p)laylists (and|&) (A|a)rchives",
    ""
  )
  djKey_raw$ShowName <- str_replace_all(djKey_raw$ShowName, "-", "")
  djKey_raw$ShowName <- str_replace_all(
    djKey_raw$ShowName,
    "(P|p)laylist|(R|r)ecent",
    ""
  )
  djKey_raw$ShowName <- str_replace_all(djKey_raw$ShowName, "WFMU|wfmu", "")
  djKey_raw$ShowName <- str_replace_all(djKey_raw$ShowName, "The ", "")
  djKey_raw$ShowName <- str_trim(djKey_raw$ShowName)

  return(djKey_raw)

  # extraction method
  # djKey_raw$other_shows[[1]] |> paste0(collapse = '\n') |> cat()
}

# -------------get the URLs of the playlist pages for a DJ ----------
#should work to delve into earlier years
get_playlist_page_URLs <- function(url_suffix) {
  #first call should be the base DJ page with links to any earlier year playlist lists
  if (str_length(url_suffix) == 2) {
    dj <- url_suffix
    latest_url <- paste0("/playlists/", url_suffix)
    url_suffix <- latest_url
  }
  singleDJ <- read_html(paste0("http://wfmu.org", url_suffix))
  #this assumes the earlier year playlist links are of the form
  # wfmu.org/playlists/<dj><year>/
  pl_url <- singleDJ %>%
    html_nodes(xpath = paste0("//a[contains(@href,'playlists/", dj, "')]")) %>%
    html_attr("href")
  # combine root with children but remove dupes and redundant URLs
  pl_url <- c(latest_url, pl_url) %>%
    unique() %>%
    str_remove_all("http.+") %>%
    stringi::stri_remove_empty()
  return(pl_url)
}


# get_playlist_page_suffixes <- function(music_djs){
#   url_suffixes <- NULL
#   for (dj in music_djs) {
#     url_suffixes <- c(url_suffixes,get_playlist_page_URLs(dj))
#   }
#   return(url_suffixes)
# }

#---------------------------------------------------
# get the URLs of the playlists for a DJ
getDJPlaylistURLs <- function(music_djs) {
  pb1 <- progress_bar$new(
    format = "  Getting Playlist URL for :what [:bar] :percent eta: :eta",
    clear = FALSE,
    total = length(music_djs)
  )

  DJ_playlists = NULL
  dudList <- NULL
  #djKey = data.frame()
  for (dj in music_djs) {
    pb1$tick(tokens = list(what = dj))
    url_suffixes <- get_playlist_page_URLs(dj)
    for (u in url_suffixes) {
      # pb$tick(tokens = list(what = str_remove(u, "/playlists/")))
      singleDJ <- read_html(paste0("http://wfmu.org", u))
      pl <- singleDJ %>%
        html_nodes(xpath = "//a[contains(@href,'playlists/shows')]") %>%
        html_attr("href")
      #format for newer shows
      pl <- as.character(na.omit(pl[str_detect(pl, "playlists/shows")]))
      # format for older shows
      if (length(pl) < 1) {
        pl <- as.character(na.omit(pl[str_detect(pl, "Playlist")]))
      }

      #assume a full URL is a fill-in DJ.  We omit these from the analysis
      pl <- pl[!str_detect(pl, "http")]

      playlistURL <- pl %>% as.character()
      #omit shows without valid playlists.  Talk shows?
      if (length(playlistURL) > 0) {
        DJ_playlists = bind_rows(
          DJ_playlists,
          tibble(DJ = dj, playlistURL = playlistURL)
        )
        dudflag <- "OK "
      } else {
        dudflag <- "DUD"
        dudList <- c(dudList, dj)
      }
    }
  }
  return(DJ_playlists)
}

# get profile page URL by extracting href containing the word "profile" from the DJ page
getDJProfileURLs <- function(DJURLs) {
  pb <- progress_bar$new(total = length(DJURLs))
  DJProfileURLs = NULL
  for (page in DJURLs) {
    singleDJ <- read_html(page)
    DJ <- sub("http://wfmu.org/playlists/", "", page)
    profileURL <- singleDJ %>%
      html_nodes(xpath = "//a[contains(@href,'profile')]") %>%
      html_attr("href")
    #    profileURL<-as.character(na.omit(profileURL[str_detect(profileURL,"profile")]))
    if (length(profileURL) > 0) {
      DJProfileURLs = bind_rows(
        DJProfileURLs,
        tibble(DJ = DJ, profileURL = profileURL)
      )
    }
    pb$tick()
  }
  return(DJProfileURLs)
}

#-------------------------------------------------
# # Get all Artists ever played by a DJ
# #WFMU maintains this as a separate page
# getDJArtistNames<-function(DJURLs) {
#   # scrape artist names for all DJs from the link at the bottom of each DJ page
#   allDJArtists<-data.frame()
#   URL_BRANCH<- "/artistkeywords.php/"
#   for (page in DJURLs) {
#     singleDJ<- read_html(page)
#     showName <- html_node(singleDJ,"title")%>%html_text()
#     showName <- gsub("\n","",sub("Playlists and Archives for ","",showName))
#     DJ <- sub("http://wfmu.org/playlists/","",page)
#     djKey<-rbind(djKey,data.frame(DJ=DJ,ShowName=showName))
#     print(showName)
#     artistListPage <- paste(ROOT_URL,URL_BRANCH,DJ, sep="")
#     artistList<-read_html(artistListPage)%>%html_node(xpath="//body/div")%>%html_text()%>%str_split("\n")
#     DJArtists<-data.frame(DJ,artistRaw=unlist(artistList))
#     if (nrow(DJArtists) >0) allDJArtists = rbind(allDJArtists,DJArtists)
#     #remove factor level of DJs with no artists
#     save(allDJArtists,file = "data/allDJArtists.rdata")
#   }
#   return(allDJArtists)
# }

#-------------- MAIN -----------------
# spoken word shows
excludeDJs <-
  sort(c(
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
    'NW',
    'GJ',
    'NP',
    'ZZ',
    'FC',
    'SY',
    'TI',
    'LK',
    'TP',
    'RC',
    'TD',
    'B3',
    'VC'
  ))
DJURLs <- getDJURLs()
# remove djurls in excludeDJs
DJURLs <- DJURLs[!str_detect(DJURLs, paste0(excludeDJs, collapse = "|"))]
djKey <- getShowNames(DJURLs)


playlistURLs <- getDJPlaylistURLs(djKey$DJ) |>
  unique()
showCounts <- playlistURLs %>%
  group_by(DJ) %>%
  summarise(showCount = n()) %>%
  arrange(desc(showCount))
djKey <- left_join(djKey, showCounts) %>%
  unique() |>
  drop_na() |>
  as_tibble()

#limit analysis to DJs with at least numShows shows.
# This also excludes DJs where we couldn't extract valid playlist URLs.
numShows <- 10
# non-music shows
djKey <- djKey %>%
  filter(showCount > numShows, !(DJ %in% excludeDJs))


compute_parquet(djKey, "data/djKey_prelim.parquet")
# save playlistURLs as parquet
compute_parquet(playlistURLs, "data/playlistURLs.parquet")
