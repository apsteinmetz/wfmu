#find umlauts in raw playlist
library(tidyverse)
umlauts <-"[öüËÄä]"
load("~/R Projects/wfmu/playlists_raw.RData")
load("~/R Projects/wfmu/djKey.RData")


# count distinct playlists
all_playlists <- playlists_raw %>% 
  group_by(DJ,AirDate) %>% 
  summarize(count=n())  %>% 
  nrow() %>% 
  {.}
# detect those with umlauts
yes_umlauts <- playlists_raw %>% 
  group_by(DJ,AirDate) %>% 
  filter(str_detect(paste0(Title,Artist),umlauts)) %>% 
  summarize(count=n())  %>% 
  nrow() %>% 
  {.}
paste0(yes_umlauts," playlists out of ",
       all_playlists,", or ",
       round(yes_umlauts/all_playlists*100),"% of playlists have umlauts")

       
# Who are the most umlaut-y DJs?
umlauts_DJs <- playlists_raw %>% 
  group_by(DJ) %>% 
  filter(str_detect(paste0(Title,Artist),umlauts)) %>% 
  summarize(umlaut_songs= n()) %>% 
  left_join(DJKey) %>% 
  transmute(ShowName,umlaut_songs,avg_umlauts_per_show=umlaut_songs/showCount) %>%
  arrange(desc(avg_umlauts_per_show))

umlauts_DJs

# most popular umlaut songs
umlaut_songs <- playlists_raw %>% 
  ungroup() %>% 
  filter(str_detect(Title,umlauts)) %>% 
  group_by(Title) %>% 
  summarize(plays=n()) %>% 
  arrange(desc(plays))

umlaut_songs  

# most popular umlaut bands
umlaut_bands <- playlists_raw %>% 
  ungroup() %>% 
  filter(str_detect(Artist,umlauts)) %>% 
  group_by(Artist) %>% 
  summarize(plays=n()) %>% 
  arrange(desc(plays))

umlaut_bands
