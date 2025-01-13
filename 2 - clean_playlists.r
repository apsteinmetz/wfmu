library(tidyverse)
library(ineq) #inequality measures
library(xts)

# try the collapse package
library(collapse)
set_collapse(mask = NULL)

#clean up raw playlists

load("data/playlists_raw.rdata")
load("data/djkey.rdata")

#Clean up inconsistent artist names

playlists<-as_tibble(playlists_raw)

playlists_raw <- playlists_raw %>% distinct()
#filter out squirrelly dates
#only Diane "Kamikaze" has archived playlists stretching back to the '80s.  Yay, Diane!
# Charlie Lewis has playlists going back to 1997 but for some reason the dates I scraped
# go way too far back for about 10 shows.  I chose to lose them since Charlie has mucho
# episodes
playlists <- playlists %>% filter(AirDate>as.Date("1982-01-01"))
playlists <-playlists %>% filter(!(AirDate<as.Date("1997-01-01")&DJ=="CL"))

# get rid of breaks
playlists <- playlists |> filter(!str_detect(Title,"Dj.Speaks"))


playlists$ArtistToken<-playlists$Artist
# one artist is all punctuation so give !!! special treatment
playlists$ArtistToken<-str_replace(playlists$ArtistToken,"!!!","chkchkchk")
# now change some common punctuation to space
print("Stripping Punctuation")
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"^\\? \\&","Question Mark And ")
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"^\\? And","Question Mark And ")
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"\\&"," ")

playlists$ArtistToken<-str_to_lower(playlists$ArtistToken)
# I choose to strip out the stuff below though dealing with it might get better analysis
#remove any text in parentheses
print("Stripping filler words")
# get rid of anything between parenthesis
#tricky regex to handle cases of multiple parentheticals in one artist
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"(\\([^(]+\\))","")
# remove 'featuring' or 'with' artists
# I chose not to remove "Versus" because that is a band name
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"(feat |featuring |and the |with |vs |vs\\.).+","")
# get rid of 'live' identifier
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"(live @ |live on|@).+","")

#now get rid of remaining non-word characters except space

playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"[^A-Z^a-z^ ^0-9]","")

#while we are at it, strip punctuantion from songs, as well
playlists$Title<-str_replace_all(playlists$Title,"[^A-Z^a-z^ ^0-9]","")

# get rid of 'interview'
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"(interview w|interview)","")

# get rid of unspecified artists
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"unknown artist(s| )|unknown","Unknown")
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"various artists|various","Unknown")

#get rid of the marathon finale
playlists<-playlists%>%filter(!str_detect(Artist,"hoof[a-zA-Z ]+sinfonia"))

#make "new york" one word.  Lots of bands start with the term
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"new york","newyork")

#make "x ray" one word. hopefully we've stripped out the dash already.Lots of bands start with the term
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"x ray","xray")

#now some connecting words that might be spelled/used variantly
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"and | of | the "," ")

#and leading "the"
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"^the "," ")
# strip leading/trailing whitespace
playlists$ArtistToken<-str_trim(playlists$ArtistToken)

#did we create any null entries
playlists<-filter(playlists,Artist!="")
playlists<-filter(playlists,Artist!="Artist")

playlists<- filter(playlists,!str_detect(Artist, "Music Behind"))
playlists<- filter(playlists,!str_detect(Title, "Music Behind"))
playlists<- filter(playlists,!str_detect(Artist, "Wake N Bake"))
playlists<- filter(playlists,!str_detect(Title, "Wfmu"))
playlists<- filter(playlists,!str_detect(Title, "Primavera"))

# get rid of wake n bake non-music plays
claylists <- playlists %>% filter(DJ=="WA")
playlists <- playlists %>% filter(DJ !="WA")

claylists<- filter(claylists,!str_detect(Artist, "^Wake "))
claylists<- filter(claylists,!str_detect(Artist, "^Pidge "))
claylists<- filter(claylists,!str_detect(Artist, "^Clay "))
playlists <- bind_rows(playlists,claylists)

numWords=2 #is two enought for uniqueness?

# we replaced all punctuation with spaces
#maybe strip spaces and combine all artist Words
#combine first two words
print("Trying to make sense of artist names")
#does this break if numWords> number of words?
playlists$ArtistToken<-playlists$ArtistToken %>% str_to_title()
t<-str_split_fixed(playlists$ArtistToken,pattern="[ ]+",n=numWords+1)[,1:numWords]
playlists$ArtistToken<-apply(t,MARGIN=1,FUN=paste,collapse=" ")

# There are a dozen ways Andy Breckman can misspell "Bruce Springsteen."
playlists<- playlists %>%   
  mutate(ArtistToken=replace(ArtistToken,
                             str_detect(ArtistToken,'BruceSp'),
                             "Springsteen"))

#Code below used for unique list of artists. Not used here.
#now that tokens are created extract unique ones for each dj so mulitples don't occur
# the zillion flavors of "Sun Ra..." will show up for each DJ only once
# not perfect.  There are a dozen ways Andy Breckman can misspell "Bruce Springsteen."
#print("Create list of unique artist names for each DJ")
#artistTokens<-playlists%>%select(DJ,artistToken)%>%group_by(DJ)%>%distinct(artistToken)


print("Combining iconic 2-name artists into one name to save space in wordcloud")
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"Rolling Stones","Stones")
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"Ennio Morricone","Morricone") #only on WFMU!
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"David Bowie","Bowie")
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"Bob Dylan","Dylan")
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"Yo La","Yo La Tengo")
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"Elvis Presley","Elvis")
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"Guided By","Guided By Voices")

#make some empty cases uniform
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"Unkown","Unknown")
#str_replace can't handle empty string pattern, so work around
playlists<-playlists %>% mutate(Title=ifelse(Title=="","Unknown",Title))
playlists<-playlists %>% mutate(ArtistToken=ifelse(ArtistToken=="","Unknown",ArtistToken))

playlists <- playlists %>% 
  filter(ArtistToken !="Your Dj") %>% 
  filter(ArtistToken !="Hoof Mouth") %>% 
  filter(ArtistToken !="Tom Wilson") %>%  #not songs
  filter(ArtistToken !="Hank Levine") %>%  #not songs
  filter(ArtistToken !="Commercial") %>%  #not songs
  distinct() %>% #why would there be dupes?  Don't know, but there are
  group_by(DJ) 

playlists <- playlists %>% mutate(across(where(is.character),str_squish))
playlists_full<-playlists


save(playlists_full,file = "data/playlists_full.rdata")
write_csv(playlists,file = "data/playlists_full.csv")

# ------------------------------------------------------------
#OPTIONAL
#using judgement to pare legitimate entries that distort analysis
print('Stripping signature songs that would distort analysis.  This takes a few minutes')
#strip out signature opening songs where one opens a show more than 20 times
#this will strip the song entirely from the database.
#should strip the artist/title pair, not the title
STRIP_THRESHOLD <- 20
playlists <- playlists_full %>%  
  mutate(artist_song=paste(ArtistToken,Title)) %>% 
  group_by(DJ,AirDate)


strip_songs <- function(playlist) {
  playlist <- playlist %>%
  summarize(FirstSong=first(artist_song)) %>%
  group_by(FirstSong) %>%
  summarise(FirstPlayCount=n()) %>%
  arrange(desc(FirstPlayCount)) %>%
  filter(FirstPlayCount>STRIP_THRESHOLD) %>%
  pull(FirstSong) %>% 
  {.}
return (playlist)
}

songs_to_strip <- strip_songs(playlists)
print(songs_to_strip)
playlists<- playlists |> 
  ungroup() |> 
  filter(!(artist_song %in% songs_to_strip)) |> 
  group_by(DJ,AirDate)
# a few DJs play TWO signature songs to open the show.  Get rid of the second one by doing it again
songs_to_strip <- strip_songs(playlists)
print(songs_to_strip)
playlists<- playlists %>% 
  ungroup() |> 
  filter(!(artist_song %in% songs_to_strip)) |> 
  group_by(DJ,AirDate)

#now strip closing songs
songs_to_strip<-playlists %>%
  summarize(FirstSong=last(artist_song)) %>%
  group_by(FirstSong) %>%
  summarise(FirstPlayCount=n()) %>%
  arrange(desc(FirstPlayCount)) %>%
  filter(FirstPlayCount>STRIP_THRESHOLD) %>%
  pull(FirstSong)
print(songs_to_strip)
playlists<- playlists %>% 
  ungroup() |> 
  filter(!(artist_song %in% songs_to_strip)) |> 
  group_by(DJ,AirDate)


#Songs where only one DJ plays it - over and over even though it might not be a signature song
#distort the analysis.  I use the Gini coefficent (used for measuring income inequality) to
# test for song/DJ concentration.  If the Gini is over 0.990, just one DJ has overwhelmingly played it.  If it
#is also in the top 200 ranking of songs over all, I strip it out.

#how aggressive should we be in scrubbing artists with lopsided appeal?
#Setting TOLERANCE to 1.000 would only filter songs with exactly one DJ accounting for all plays.
# I have set this to 0.997 which essentially deprecates the function because the show, Greasy
# Kid stuff played a few songs an awful lot but I didn't want to lose the greatest hits.
TOLERANCE <- 0.997
NUM_DJS<- length(unique(playlists$DJ))

song_conc<-function(song){
  g<-playlists %>% 
    ungroup() %>% 
    filter(artist_song==song) %>% 
    select(DJ,artist_song) %>% 
    group_by(DJ) %>%
    summarise(n=n()) %>% 
    arrange(desc(n)) %>% 
    pull(n) %>% 
    c(rep(0,NUM_DJS)) %>% .[1:NUM_DJS] %>% #pad to include no-play DJs in Gini calc
    ineq::Gini()
  return (g)
}

count_by_song<-playlists %>%
  ungroup() %>% 
  group_by(artist_song) %>% 
  summarise(Song_Count=n()) %>% 
  arrange(desc(Song_Count))

print('Computing DJ concentration of most-played songs')
songs_to_strip<-NULL
for (n in 1:200){
  print(n)
  song<-count_by_song$artist_song[n]
  gini<-song_conc(song)
  if (gini > TOLERANCE){
    songs_to_strip<-c(songs_to_strip,song)
  }
  
}
print("Stripping")
print(songs_to_strip)

playlists<- playlists %>% 
  ungroup() |> 
  filter(!(artist_song %in% songs_to_strip)) |> 
  group_by(DJ,AirDate)

# save the results
playlists<-playlists %>% select(-artist_song) # remove before saving. much smaller file

save(playlists,file = "data/playlists.rdata")

#get a better show count tally
show_count<-playlists %>% 
  ungroup() |> 
  distinct(DJ, AirDate) %>% 
  group_by(DJ) |> 
  summarise(showCount=n())

DJKey<-DJKey %>% select(-showCount) %>% left_join(show_count) %>% distinct()
save(DJKey,file = "data/DJKey.rdata")


#use artisttoken to select the most common version of the artist name and make that the token.
playlists<-playlists %>% 
  ungroup() %>% 
  select(ArtistToken,Artist) %>% 
  group_by(ArtistToken,Artist) %>% 
  summarise(n=n()) %>% 
  top_n(1) %>% 
  rename(base_artist=Artist) %>% 
  right_join(playlists,by='ArtistToken') %>%
  ungroup() %>% 
  select(-ArtistToken,-n) %>% 
  rename(ArtistToken=base_artist)


#test section
#clean it again
#get rid of punctution
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"[^A-Z^a-z^ ^0-9]","")
#and leading "the"
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"^The ","")

# strip leading/trailing whitespace and unicode that LaTex balks at
playlists <- playlists %>% 
  mutate_if(is.character,str_remove_all,"[\u236-\u400E]") %>% 
  mutate_if(is.character,str_squish)

save(playlists,file = "data/playlists.rdata")
#write_csv(playlists,path="playlists.csv")

# add first show and last show to djkey
FirstShow<-playlists %>% 
  group_by(DJ) %>% 
  select(DJ,AirDate) %>% 
  distinct() %>% 
  top_n(-1) %>% rename(FirstShow=AirDate)

LastShow<-playlists %>% 
  group_by(DJ) %>% 
  select(DJ,AirDate) %>% 
  distinct() %>% 
  top_n(1) %>% rename(LastShow=AirDate)

DJKey <- DJKey %>% 
  select(DJ,ShowName,onSched,showCount) %>% 
  left_join(FirstShow,by=c("DJ")) %>% 
  left_join(LastShow,by=c("DJ"))

save(DJKey, file = "data/DJKey.RData")


