# get playlists of single DJ by episode name
library(tidyverse)
library(duckplyr)
library(rvest)
methods_overwrite()
# efd playlist list
efd_shows_url <- "https://wfmu.org/playlists/ED"
plURL  <- "/playlists/ED"
dj_code <- "ED"
ROOT_URL <- "https://wfmu.org"


# get playlist links
playlist_nodes <- efd_shows_url |> 
  read_html() |> 
  html_nodes("li")


get_show_metadata <- function(playlist_node){
  episode_name <-  playlist_node |> 
    html_node("b") |> 
    html_text()
  AirDate <- playlist_node |>
    #find in string date in form of month day, year
    html_text() |>
    str_extract("\\w+ \\d+, \\d{4}") |>
    as.Date(format = "%B %d, %Y")
  episode_url <- playlist_node |>
    html_node(xpath="//a[contains(@href,'playlists/shows')]") %>%
    html_attr("href")
  episode_url <- paste0(ROOT_URL,episode_url)
  return(tibble(episode_name, AirDate, episode_url))
}  
  
efd_shows <- playlist_nodes |> 
  map(get_show_metadata) |> bind_rows()

PPPNW <- efd_shows |> 
  filter(str_detect(episode_name,"punk|Punk|PUNK"))

disco <- efd_shows |> 
  filter(str_detect(episode_name,"DISCO|Disco|disco")) |> 
  filter(!str_detect(episode_name,"Kenny"))

seventies <- efd_shows |> 
  filter(str_detect(episode_name,"SEVENTIES|Seventies|seventies"))

episodes = seventies


get_theme_show_songs <- function(episodes,suffix=""){

  # efd <- duckplyr_df_from_file("data/playlists.parquet","read_parquet") |> 
  efd <- as_duckplyr_df(playlists_raw) |>
    filter(DJ == dj_code) |>
    right_join(episodes) |>
    mutate(Artist = str_remove(Artist, "Music Behind Dj:")) |>
    mutate(Artist = str_remove_all(Artist, '"')) |>
    mutate(Artist = str_remove_all(Artist, "'")) |>
    mutate(Artist = str_replace(Artist, "&|\\+", "and")) |>
    as_tibble()
  
songs <- efd |> 
  summarise(.by = c(Artist, Title),plays = n()) |> 
  arrange(Artist)

#artists <- efd |> 
#  summarise(.by = c(Artist),plays = n(),songs=list(unique(Title))) |> 
#  arrange(desc(plays))
write.csv(songs,file=paste0("data/efd_songs_",suffix,".csv"))
}

get_theme_show_songs(disco,"PPPNW")
get_theme_show_songs(disco,"seventies")
get_theme_show_songs(disco,"disco")

