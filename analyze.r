#analyze list of artists to find DJ similarity


#force git to overwrite local files on pull. run from git shell
#git fetch --all
#git reset --hard origin/master

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
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\*"," ")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,'\\['," ")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,'\\]'," ")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,'[!:"/+,]'," ")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\'"," ")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\&"," ")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\."," ")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\#"," ")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\-"," ")
  
  allDJArtists$artist<-str_to_lower(allDJArtists$artist)
  # I choose to strip out the stuff below though dealing with it might get better analysis
  #remove parentheticals
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\(.*\\)","")
  # remove 'featuring' or 'with' artists
  # I chose not to remove "Versus" because that is a band name
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"(feat |featuring |with |vs |vs\\.).+","")
  # get rid of 'live' identifier
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"(live @ |live on|@).+","")
  # get rid of 'interview'
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"(interview w|interview)","")
  #get rid of the marathon finale
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"hoof mouth sinfonia","")
  
  # get rid of unspecified artists
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"unknown artist(s| )|unknown","")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"various artists|various","")
  
    #now some connecting words that might be spelled/used variantly
  # first make all lower cast
  print("Stripping filler words")
  joinWords <- c("and ","the ","of ")
  for (w in joinWords){
    allDJArtists$artist<-str_replace_all(allDJArtists$artist,w," ")
  }  
  # strip leading/trailing whitespace
  allDJArtists$artist<-str_trim(allDJArtists$artist)

  #did we create any null entries
  allDJArtists<-filter(allDJArtists,artist!="")
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

  #now that tokens are created extract unique ones for each dj so mulitples don't occur
  # the zillion flavors of "Sun Ra..." will show up for each DJ only once
  # not perfect.  There are a dozen ways Andy Breckman can misspell "Bruce Springsteen."
  artistTokens<- data.frame()
  for (dj in levels(allDJArtists$DJ)){
    print(dj)
    t<-allDJArtists%>%filter(DJ==dj)%>%select(artistToken2)%>%unique()
    if (nrow(t) >0) artistTokens<-rbind(artistTokens,data.frame(dj=dj,artistToken2=t))
  }
    
}

combineAllArtists <- function(){
  t<- data.frame()
  for (dj in levels(artistTokens$dj)){
    #put all words in string for each DJ
    print("Creating artist documents")
  print(dj)  
  t<-rbind(t,data.frame(DJ=dj,artists= artistTokens$artistToken2%>%
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


# test<-filter(allDJArtists,DJ=="GK")
djDocs<-combineAllArtists()
save(djDocs,file="djDocs.RData")

load("djDocs.RData")

#docs <- data.frame(author=c("me","you"),texts=c("This is a text.", "This another one."))
djCorpus <- Corpus(VectorSource(djDocs$artists))

for (i in 1:length(djCorpus)) {
  meta(djCorpus[[i]], tag="DJ") <- djDocs$DJ[i]
}

#make a word cloud
#djdtm<-DocumentTermMatrix(djCorpus)
#for wordcloud of most widely played artists
djtdm<-TermDocumentMatrix(djCorpus)
m <- as.matrix(djtdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

