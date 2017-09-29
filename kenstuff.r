#make Ken trifecta
# The ideal size for the homepage slider is 710 pixels wide by 355 pixels high --Ken
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lubridate)
load(file='playlists.RData')

artist_troika<-c("OhSees","Funkadelic","JohnnyCash")

troika<-playlists %>% 
  ungroup() %>% 
  mutate(AirDate=year(AirDate)) %>% 
  filter(AirDate>2005) %>% 
  filter(ArtistToken %in% artist_troika) %>% 
  group_by(AirDate,ArtistToken) %>% 
  summarise(PlayCount=n())

gg<-troika %>% ggplot(aes(as.factor(AirDate),PlayCount,fill=ArtistToken))+geom_col()
gg<-gg+theme_economist() + labs(x="Air Date",y="Play Count") +
  theme(axis.title = element_text(size = 12), 
        legend.title = element_text(size = 12,face='italic'))
gg

gg<-troika %>% ggplot(aes(as.factor(AirDate),PlayCount,fill=ArtistToken))+geom_col(position = 'dodge')
gg<-gg+theme_economist() + labs(x="Air Date",y="Play Count") +
  theme(axis.title = element_text(size = 12), 
        legend.title = element_text(size = 12,face='italic'))
gg

gg+geom_col(position='dodge')

gg+facet_wrap(~ArtistToken) + theme(axis.text.x = element_text(angle = 90))
