# chatGPT code to get metadata for a github repo file
library(dplyr)
library(curl)
library(jsonlite)

get_github_file_date <- function(owner='apsteinmetz', repo='wfmu', filepath='README.md') {
  # Construct the API URL
  url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/commits?path=", filepath)
  response <-fromJSON(curl(url),flatten=TRUE)
  metadata <-
    response  |> as_tibble() |> head(1) |> pull(commit.author.date) |> as.POSIXct()
  return(metadata)
}


refresh_parquet_data <- function(filepath){
  # download from github if data in the cloud is new than local copy
  local_date <- pull(fs::file_info(filepath),modification_time)
  local_date <- ifelse (is.na(local_date),0,local_date)
  remote_date <- get_github_file_date(filepath = filepath)
  if (remote_date >local_date){
    print(paste0(filepath," :Remote file is newest. Downloading"))
    download.file(paste0('https://github.com/apsteinmetz/wfmu/raw/master/',filepath),
                  mode = "wb",
                  destfile = filepath)
  } else print(paste0(filepath," :Local file is up to date."))
  # otherwise load local copy
  playlists <- arrow::read_parquet(file=filepath,as_data_frame = FALSE)
  
}

filepath <- "data/playlists.parquet"
refresh_parquet_data(filepath)

