#analyze list of artists to find DJ similarity

# # install.packages("devtools")
# # require(devtools)
# # install_url("http://www.omegahat.org/Rstem/Rstem_0.4-1.tar.gz")
# # install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
# # install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
# 
library(stringr)
library(tm)
library(dplyr)
library("ggplot2")
library("wordcloud")
library("RColorBrewer")
library("SnowballC")
#library(sentiment) not available



cleanUpArtists<- function() {

# one artist is all punctuation so give !!! special treatment
  allDJArtists$artist<-str_replace(allDJArtists$artist,"!!!","chkchkchk")
  # now change some common punctuation to space
  #I'm sure there is a more elegant way to do this.
  print("Stripping Punctuation")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\("," ")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\)"," ")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\*"," ")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,'\\['," ")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,'\\]'," ")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,'[!:"/+-,]'," ")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\'"," ")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\&"," ")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\."," ")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\#"," ")
  allDJArtists$artist<-str_trim(allDJArtists$artist)
  #did we create any null entries
  allDJArtists<-filter(allDJArtists,artist!="")
  
  #now some connecting words that might be spelled/used variantly
  # first make all lower cast
  print("Stripping ambiguous words")
  allDJArtists$artist<-str_to_lower(allDJArtists$artist)
  joinWords <- c("with","feat","featuring","vs","versus","and","the")
  for (w in joinWords){
    allDJArtists$artist<-str_replace_all(allDJArtists$artist,w," ")
  }  

}



combineArtistWords <- function(){
  # we replaced all punctuation with spaces
  #maybe strip spaces and combine all artist Words
  artistToken1<-str_replace_all(allDJArtists$artist," ","")
}

combineTwoArtistWords <- function(){
  # we replaced all punctuation with spaces
  #maybe strip spaces and combine all artist Words
  #combine first two words
  print("Trying to make sense of artist names")
  t<-str_split_fixed(allDJArtists$artist,pattern="[ ]+",n=3)[,1:2]
  allDJArtists$artistToken2<-apply(t,MARGIN=1,FUN=paste,collapse="")
}

combineAllArtists <- function(){
  t<- data.frame()
  for (dj in levels(allDJArtists$DJ)){
    #put all words in string for each DJ
    print("Creating artist documents")
  print(dj)  
  t<-rbind(t,data.frame(DJ=dj,artists= allDJArtists%>%
                          filter(DJ==dj)%>%
                          select(artistToken2)%>% 
                          unlist()%>%paste(collapse=" ")))
             }
  
  return(t)
}

# ----------------- MAIN --------------
load("allDJArtists.RData")
cleanUpArtists()
#expiriment with different algorithms to uniquely identify artist
combineTwoArtistWords()

#combine words into one document per DJ


test<-filter(allDJArtists,DJ=="GK")
djDocs<-combineAllArtists()

# maybe use longest word in artist name as having the most info content
findLongestWord(test$artist[1])

sentence<-test$artist[1]
