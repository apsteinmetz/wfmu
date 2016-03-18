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
library(igraph)


cleanUpArtists<- function(allDJArtists) {
  allDJArtists$artistRaw<-allDJArtists$artist
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
  #remove any text in parentheses
  print("Stripping filler words")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\(.*\\)","")
  # remove 'featuring' or 'with' artists
  # I chose not to remove "Versus" because that is a band name
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"(feat |featuring |with |vs |vs\\.).+","")
  # get rid of 'live' identifier
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"(live @ |live on|@).+","")
  # get rid of 'interview'
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"(interview w|interview)","")
  
  # get rid of unspecified artists
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"unknown artist(s| )|unknown","")
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"various artists|various","")
  
  #get rid of the marathon finale
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"hoof mouth sinfonia|ho mouth sinfonia","")
  
  #make "new york" one word.  Lots of bands start with the term
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"new york","newyork")
  
  #make "x ray" one word. hopefully we've stripped out the dash already.Lots of bands start with the term
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"x ray","xray")
  
  #now some connecting words that might be spelled/used variantly

  joinWords <- c("and ","the ","of ")
  for (w in joinWords){
    allDJArtists$artist<-str_replace_all(allDJArtists$artist,w," ")
  }  
  # strip leading/trailing whitespace
  allDJArtists$artist<-str_trim(allDJArtists$artist)

  #did we create any null entries
  allDJArtists<-filter(allDJArtists,artist!="")
  
  return(allDJArtists)
  
}



#combineArtistWords <- function(){
  # we replaced all punctuation with spaces
  #maybe strip spaces and combine all artist Words
#  artistToken1<-str_replace_all(allDJArtists$artist," ","")
#}

combineArtistWords <- function(allDJArtists,numWords){
  # we replaced all punctuation with spaces
  #maybe strip spaces and combine all artist Words
  #combine first two words
  print("Trying to make sense of artist names")
  #does this break if numWords> number of words?
  t<-str_split_fixed(allDJArtists$artist,pattern="[ ]+",n=numWords+1)[,1:numWords]
  
  allDJArtists$artistToken2<-apply(t,MARGIN=1,FUN=paste,collapse="")

  

  #now that tokens are created extract unique ones for each dj so mulitples don't occur
  # the zillion flavors of "Sun Ra..." will show up for each DJ only once
  # not perfect.  There are a dozen ways Andy Breckman can misspell "Bruce Springsteen."
  print("Create list of unique artist names for each DJ")
  artistTokens<- data.frame()
  for (dj in levels(allDJArtists$DJ)){
    print(dj)
    t<-allDJArtists%>%filter(DJ==dj)%>%select(artistToken2)%>%unique()
    if (nrow(t) >0) artistTokens<-rbind(artistTokens,data.frame(DJ=dj,artistToken2=t))
  }
  # some very popular artists can have their names shortened to make more room
  # on the wordcloud because you know exactly who we're talking about
  artistTokens$artistToken2<-str_replace_all(artistTokens$artistToken2,"rollingstones","stones")
  artistTokens$artistToken2<-str_replace_all(artistTokens$artistToken2,"enniomorricone","morricone") #only on WFMU!
  artistTokens$artistToken2<-str_replace_all(artistTokens$artistToken2,"davidbowie","bowie")
  artistTokens$artistToken2<-str_replace_all(artistTokens$artistToken2,"bobdylan","dylan")
  #rm(t)
  return(artistTokens)
}

combineAllArtists <- function(){
  t<- data.frame()
  for (dj in levels(artistTokens$DJ)){
    #put all words in string for each DJ
    print(paste("Creating artist documents",dj))
  t<-rbind(t,data.frame(DJ=dj,artists= artistTokens%>%
                          filter(DJ==dj)%>%
                          select(artistToken2)%>% 
                          unlist()%>%paste(collapse=" ")%>%
                          str_replace_all("[^a-z ]","")%>%as.character()))
  }
  #artists should not have factor levels
  t$artists<-as.character(t$artists)
  return(t)
}

# ----------------- MAIN --------------
load("allDJArtists.RData")
allDJArtists<-cleanUpArtists(allDJArtists)
#expiriment with different algorithms to uniquely identify artist

#combine first numWords words in artist name into a single token
artistTokens<-combineArtistWords(allDJArtists,numWords=2)
#save(artistTokens,file="artistTokens.RData")

#combine words into one document per DJ

load("artistTokens.RData")
djDocs<-combineAllArtists()
save(djDocs,file="djDocs.RData")

load("djDocs.RData")

djCorpus <- Corpus(VectorSource(djDocs$artists))

for (i in 1:length(djCorpus)) {
  meta(djCorpus[[i]], tag="id") <- djDocs$DJ[i]
}

#make a word cloud
#for wordcloud of most widely played artists
#removing sparse terms at 0.99 means that artists played by less than 3 DJs will be dropped

djtdm<-TermDocumentMatrix(djCorpus)%>%removeSparseTerms(0.80)


m <- as.matrix(djtdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
t<-head(d, 200)
rownames(t)<-NULL
print(t)
save(t,file='artistfreq.txt',ascii = TRUE)

#scalefactor magnifies differences for wordcloud
scaleFactor=3
wordcloud(words = t$word, freq = t$freq^scaleFactor,max.words=100, random.order=FALSE,rot.per=0.35, 
             colors=brewer.pal(8, "Dark2"),scale = c(3,.2))



#dd <- as.dist((1 - cor(t(m)))/2)
#network graph
ga<-graph.adjacency(t(m))

#put DJs in rows, artists in columns
#get roughly top 400 artists when removeSparseTerms(0.80) used. top 8000 when 0.95 sparse is used
djdtm<-DocumentTermMatrix(djCorpus) %>%removeSparseTerms(0.8)
m2<-as.matrix(djdtm)
rownames(m2)<-djDocs$DJ
save(m2,file="docTermMatrix.RData")

#create document matrix of commonalities
docMatrix<-m2 %*% t(m2)
# get rid of DJs with no association to anybody after making matrix sparse
# if complete matrix is used this will have no effect
orphans<-row.names(docMatrix[rowSums(docMatrix)==0,])
docMatrix2<-docMatrix[rowSums(docMatrix)!=0,rowSums(docMatrix)!=0]

library(igraph)
# build a graph from the above matrix
g <- graph.adjacency(docMatrix2, weighted=T, mode = "undirected")
 # remove loops
g <- simplify(g)
#set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)
gefx<-igraph.to.gexf(g)
print(wfmugraf,file="wfmugraf.gexf")

