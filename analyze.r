#analyze list of artists to find DJ similarity

# # install.packages("devtools")
# # require(devtools)
# # install_url("http://www.omegahat.org/Rstem/Rstem_0.4-1.tar.gz")
# # install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
# # install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
# 
library(stringr)
library(tm)
library("plyr")
library("ggplot2")
library("wordcloud")
library("RColorBrewer")
library("SnowballC")
library(sentiment)


load("allDJArtists.RData")


#needs work
findLongestWord <- function(sentence) {
  len <-0
  wlen <- 0
  longWord = ""
  listOfWords = str_split(sentence," ")
  #find longest word in a sentence
  for (w in listOfWords) {
    len <- str_length(w)
    if (len >wlen) {
      longWord=w
      wlen <- len 
    }
  }
  return(longWord)
}

#expiriment with different algorithms to uniquely identify artist
# one artist is all punctuation so give !!! special treatment
allDJArtists$artist<-str_replace(allDJArtists$artist,"!!!","chkchkchk")
# now change some common punctuation to space
allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\("," ")
allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\)"," ")
allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\*"," ")
allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\*"," ")
allDJArtists$artist<-str_replace_all(allDJArtists$artist,'\\['," ")
allDJArtists$artist<-str_replace_all(allDJArtists$artist,'\\]'," ")
allDJArtists$artist<-str_replace_all(allDJArtists$artist,'[!:"/+-]'," ")
allDJArtists$artist<-str_replace_all(allDJArtists$artist,'[!:"/+-]'," ")

# use longest word in artist name as having the most info content
test<-filter(allDJArtists,DJ=="GK")
test[1,2]
findLongestWord(test$artist[1])

sentence<-test$artist[1]

#put all words in string for each DJ
paste(test[,2],collapse=" ")