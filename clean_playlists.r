library(dplyr)
library(stringr)
library(readr)
#clean up raw playlists

load("playlists_raw.Rdata")
load("djkey.Rdata")

#Clean up inconsistant artist names

playlists<-playlists_raw %>% 
  distinct() %>% 
  filter(Artist != Title) #single column span across table.  Not a song.

#filter out squirrelly dates
#only Diane "Kamikaze" has archived playlists stretching back to the '80s.  Yay, Diane!
# Charlie Lewis has playlists going back to 1997 but for some reason the dates I scraped
# go way too far back for about 10 shows.  I chose to lose them since Charlie has mucho
# episodes
playlists <- playlists %>% filter(AirDate>as.Date("1982-01-01"))

playlists$ArtistToken<-playlists$Artist
# one artist is all punctuation so give !!! special treatment
playlists$ArtistToken<-str_replace(playlists$ArtistToken,"!!!","chkchkchk")
# now change some common punctuation to space
print("Stripping Punctuation")

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

playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"[^a-z^ ^0-9]","")


# get rid of 'interview'
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"(interview w|interview)","")

# get rid of unspecified artists
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"unknown artist(s| )|unknown","")
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"various artists|various","")

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

playlists<- filter(playlists,!str_detect(Artist, "Music Behind Dj:"))
playlists<- filter(playlists,!str_detect(Artist, "Wake N Bake"))


numWords=2 #is two enought for uniqueness?

# we replaced all punctuation with spaces
#maybe strip spaces and combine all artist Words
#combine first two words
print("Trying to make sense of artist names")
#does this break if numWords> number of words?
t<-str_split_fixed(playlists$ArtistToken,pattern="[ ]+",n=numWords+1)[,1:numWords]

playlists$ArtistToken<-apply(t,MARGIN=1,FUN=paste,collapse="")

# There are a dozen ways Andy Breckman can misspell "Bruce Springsteen."
playlists<- playlists %>%   
  mutate(ArtistToken=replace(ArtistToken,
                             str_detect(ArtistToken,'brucesp'),
                             "springsteen"))

#Code below used for unique list of artists. Not used here.
#now that tokens are created extract unique ones for each dj so mulitples don't occur
# the zillion flavors of "Sun Ra..." will show up for each DJ only once
# not perfect.  There are a dozen ways Andy Breckman can misspell "Bruce Springsteen."
#print("Create list of unique artist names for each DJ")
#artistTokens<-playlists%>%select(DJ,artistToken)%>%group_by(DJ)%>%distinct(artistToken)


print("Combining iconic 2-name artists into one name to save space in wordcloud")
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"rollingstones","stones")
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"enniomorricone","morricone") #only on WFMU!
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"davidbowie","bowie")
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"bobdylan","dylan")
playlists$ArtistToken<-str_replace_all(playlists$ArtistToken,"yola","yolatengo")

playlists<-playlists %>% 
  mutate(DJ=as.factor(DJ)) %>%
  filter(ArtistToken !="") %>% 
  filter(ArtistToken !="yourdj") %>% 
  filter(ArtistToken !="hoofmouth") %>% 
  filter(ArtistToken !="tomwilson") %>%  #not songs
  filter(ArtistToken !="hanklevine") %>%  #not songs
  filter(ArtistToken !="commerical") %>%  #not songs
  distinct() %>% #why would there be dupes?  Don't know, but there are
  group_by(DJ) 

#strip out signature opening songs where one opens a show more than 20 times
#this will strip the song entirely from the database.
#should strip the artist/title pair, not the title
playlists <- playlists %>%  mutate(Artist_Song=paste(ArtistToken,Title)) 

songs_to_strip<-playlists %>% 
  group_by(DJ,AirDate) %>% 
  summarize(FirstSong=first(Artist_Song)) %>% 
  group_by(FirstSong) %>% 
  summarise(FirstPlayCount=n()) %>% 
  arrange(desc(FirstPlayCount)) %>%
  filter(FirstPlayCount>20) %>% 
  pull(FirstSong)

playlists<- playlists %>% filter(!(Artist_Song %in% songs_to_strip))

write_csv(playlists,path="playlists.csv")
save(playlists,file="playlists.Rdata")
