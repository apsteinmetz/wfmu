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
library(devtools)
#install_github('sinhrks/ggfortify')
library(ggplot2)
library(stringr)
library(tm)
library(dplyr)
library("ggplot2")
#library(ggfortify)
library("wordcloud")
library("RColorBrewer")
library("SnowballC")
library(igraph)
library(cluster)
library(reshape2)
library(proxy)
library(Matrix)

#get roughly top 400 artists when removeSparseTerms(0.80) used. top 8000 when 0.95 sparse is used
SPARSE<- 0.95 #sparsity of term document matrices

cleanUpArtists<- function(allDJArtists) {
  allDJArtists$artistRaw<-allDJArtists$artist
# one artist is all punctuation so give !!! special treatment
  allDJArtists$artist<-str_replace(allDJArtists$artist,"!!!","chkchkchk")
  # now change some common punctuation to space
  print("Stripping Punctuation")

  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"\\&"," ")
  
  allDJArtists$artist<-str_to_lower(allDJArtists$artist)
  # I choose to strip out the stuff below though dealing with it might get better analysis
  #remove any text in parentheses
  print("Stripping filler words")
  # get rid of anything between parenthesis
  #tricky regex to handle cases of multiple parentheticals in one artist
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"(\\([^(]+\\))","")
  # remove 'featuring' or 'with' artists
  # I chose not to remove "Versus" because that is a band name
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"(feat |featuring |with |vs |vs\\.).+","")
  # get rid of 'live' identifier
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"(live @ |live on|@).+","")

  #now get rid of remaining non-word characters except space

  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"[^a-z^ ^0-9]","")
  
  
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
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"and | of | the "," ")

  #and leading "the"
  allDJArtists$artist<-str_replace_all(allDJArtists$artist,"^the "," ")
  # strip leading/trailing whitespace
  allDJArtists$artist<-str_trim(allDJArtists$artist)

  #did we create any null entries
  allDJArtists<-filter(allDJArtists,artist!="")
  allDJArtists<-filter(allDJArtists,artist!="artist")
  
  return(allDJArtists)
  
}
#-----------------------------------------------------------------------------
combineArtistWords <- function(allDJArtists,numWords){
  # we replaced all punctuation with spaces
  #maybe strip spaces and combine all artist Words
  #combine first two words
  print("Trying to make sense of artist names")
  #does this break if numWords> number of words?
  t<-str_split_fixed(allDJArtists$artist,pattern="[ ]+",n=numWords+1)[,1:numWords]
  
  allDJArtists$artistToken<-apply(t,MARGIN=1,FUN=paste,collapse="")

  #now that tokens are created extract unique ones for each dj so mulitples don't occur
  # the zillion flavors of "Sun Ra..." will show up for each DJ only once
  # not perfect.  There are a dozen ways Andy Breckman can misspell "Bruce Springsteen."
  print("Create list of unique artist names for each DJ")
  artistTokens<-allDJArtists%>%select(DJ,artistToken)%>%group_by(DJ)%>%distinct(artistToken)

  print("Combining iconic 2-name artists into one name to save space in wordcloud")
  artistTokens$artistToken<-str_replace_all(artistTokens$artistToken,"rollingstones","stones")
  artistTokens$artistToken<-str_replace_all(artistTokens$artistToken,"enniomorricone","morricone") #only on WFMU!
  artistTokens$artistToken<-str_replace_all(artistTokens$artistToken,"davidbowie","bowie")
  artistTokens$artistToken<-str_replace_all(artistTokens$artistToken,"bobdylan","dylan")
  #rm(t)
  return(artistTokens)
}
#-------------------------------------------------------------------------------------
addArtistCount<- function(DJKey,artistTokens) {
  DJKey<-artistTokens%>%summarise(count=n())%>%inner_join(DJKey)%>%arrange(desc(count))
  names(DJKey)<-c("DJ","artistCount","ShowName","onMic")
  # make DJs a factor
  DJKey$ShowName<-factor(DJKey$ShowName,as.character(DJKey$ShowName))
  DJKey$DJ<-factor(DJKey$DJ,as.character(DJKey$DJ))
  return(DJKey)
}
#-------------------------------------------------------------  
combineAllArtists <- function(){
  t<- data.frame()
  #make sure there aren't extra levels
  artistTokens$DJ<-factor(artistTokens$DJ,as.character(unique(artistTokens$DJ)))
  for (dj in levels(artistTokens$DJ)){
    #put all words in string for each DJ
    print(paste("Creating artist documents",dj))
  t<-rbind(t,data.frame(DJ=dj,
                        artists= artistTokens%>%
                          filter(DJ==dj)%>%
                          select(artistToken)%>% 
                          unlist()%>%paste(collapse=" ")%>%
                          str_replace_all("[^a-z ]","")%>%as.character(),
                        onMic=DJKey%>%filter(DJ==dj)%>%select(onMic)))
  }
  #artists should not have factor levels
  t$artists<-as.character(t$artists)
  t<-filter(t,artists!="")
  return(t)
}
#------------------------------------------------------------
delete.isolates <- function(graph) {
  #isolates <- which(degree(graph, mode = mode) == 0) - 1
  #delete.vertices(graph, isolates)
  return(delete.vertices(graph,V(graph)[degree(graph)==0]))
}
#-----------------------------------------------------------
jaccard <- function(m) {
  #http://stackoverflow.com/questions/36220585/efficient-jaccard-similarity-documenttermmatrix
  ## common values:
  A <- tcrossprod(m)
  im <- which(A > 0, arr.ind=TRUE, useNames = FALSE)
  b <- rowSums(m)
  Aim <- A[im]
  J<-sparseMatrix(
    i = im[,1],
    j = im[,2],
    x = Aim / (b[im[,1]] + b[im[,2]] - Aim),
    dims = dim(A)
  )
  
  #preserve row/column names, if any
  rownames(J)<-rownames(m)
  colnames(J)<-rownames(m)
  return( J )
}  
#-----------------------------------------------------------
getSimilarity<-function(djdtm=djdtm){
  m2<-as.matrix(djdtm)
  rownames(m2)<-djDocs$DJ

  # get similarity
  #j<-simil(m2,method="Jaccard")
  j<-jaccard(m2)
  save(j,file="djSimilarity.RData")
  return(j)
}
#-----------------------------------------------------------
assignClusters<-function(j,CLUSTERS=5) {

  #j<-getSimilarity(djCorpus)
  
  # find similarity clusters
  #make a plot
  set.seed(1)
  kdj<-kmeans(j,CLUSTERS)
  clust<-kdj$cluster
  #djCluster<-cbind(DJ=names(clust),data.frame(cluster=clust))
  
  #if ("cluster" %in% names(DJKey)){
  #  # then remove the cluster column
  #  DJKey<-select(DJKey,-cluster)
  #}
  # now replace with new clustering
  #DJKey<-inner_join(DJKey,djCluster)%>%arrange(cluster)
    
  
  clusplot(as.matrix(j), clust, main="DJ Similiarity Clusters",color=T, shade=T, labels=2, lines=0) 
  #autoplot(kdj,data=j)
  #list DJ clusters
  #return (clust)
}
#----------------------------------------------------------
similarDJs<-function(whichDJ="TW",compareDJ="TM",j,DJkey,artistTokens){
  #djs for example
  #whichDJ <- "TW"
  #compareDJ<-"TM"
  likeDJs<-data.frame(similarity=as.matrix(j)[whichDJ,])
  #likeDJs<-likeDJs%>%add_rownames(var="DJ")
  likeDJs$DJ<-rownames(likeDJs)
  likeDJs<-arrange(likeDJs,desc(similarity))[-1,] #sort descending and remove self DJ
  rownames(likeDJs)<-NULL
  #since we sorted in order of descending similarity the first row has the most similar DJ
  compareDJ<-likeDJs[1,"DJ"]
  whichShow<-DJKey%>%filter(DJ==whichDJ)%>%.$ShowName%>%as.character()
  compareShow<-DJKey%>%filter(DJ==compareDJ)%>%.$ShowName%>%as.character()
  paste(whichShow,"is most similar to",compareShow)
  paste("Similarity Index:",format(likeDJs[1,"similarity"],digits=2),"/1.00")
  commonArtists<-intersect(artistTokens[which(artistTokens$DJ==whichDJ),]$artistToken,artistTokens[which(artistTokens$DJ==compareDJ),]$artistToken)
  print(as.data.frame(sample(commonArtists,20)))
}
#------------------------------------------------------------
#not functional
plotNetwork <- function(docMatrix) {
  library(proxy)
  library(igraph)
  #put DJs in rows, artists in columns
  idx <- meta(djCorpus, "onMic") == TRUE
  djdtm<-DocumentTermMatrix(djCorpus[idx]) %>% removeSparseTerms(0.8)
  m2<-as.matrix(djdtm)
  rownames(m2)<-djDocs$DJ[which(djDocs$onMic==TRUE)]
  save(m2,file="docTermMatrix.RData")
  
  #get euclidean distance
  # d<-dist(m2)
  #j<-jaccard(m2)
  j<-simil(m2,method="Jaccard")
  
  
  # find similarity clusters
  CLUSTERS<-5
  #make a plot
  clust<-kmeans(j,CLUSTERS)$cluster
  djCluster<-cbind(DJ=names(clust),data.frame(cluster=clust))
  temp<-arrange(inner_join(DJKey,djCluster),cluster)

  
  clusplot(as.matrix(j), clust, main="DJ Similiarity Clusters",color=T, shade=T, labels=2, lines=0) 
  #list DJ clusters
  
  #djs for example
  whichDJ <- "TW"
  compareDJ<-"TM"
  likeDJs<-data.frame(similarity=as.matrix(j)[whichDJ,])
  #likeDJs<-likeDJs%>%add_rownames(var="DJ")
  likeDJs$DJ<-rownames(likeDJs)
  likeDJs<-arrange(likeDJs,desc(similarity))[-1,] #sort descending and remove self DJ
  rownames(likeDJs)<-NULL  
  
  commonArtists<-intersect(artistTokens[which(artistTokens$DJ==whichDJ),]$artistToken,artistTokens[which(artistTokens$DJ==compareDJ),]$artistToken)
  sample(commonArtists,20)
  
  
  # create graph
  g<-as.matrix(j)%>%melt()%>%as.data.frame()%>%graph.data.frame()
  
  #create document matrix of commonalities
  #docMatrix<-m2 %*% t(m2)
  # get rid of DJs with no association to anybody after making matrix sparse
  # if complete matrix is used this will have no effect
  #orphans<-row.names(docMatrix[rowSums(docMatrix)==0,])
  #print(orphans)
  #docMatrix<-docMatrix[rowSums(docMatrix)!=0,rowSums(docMatrix)!=0]
  # build a graph from the above matrix
  #g <- graph.adjacency(docMatrix, weighted=T, mode = "undirected")
  # remove loops
  g <- simplify(g)
  #set labels and degrees of vertices
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  E(g)$weight<-E(g)$value
  
  #remove edges with similarity less than 0.1 Jaccard
  g2 <- delete.edges(g, which(E(g)$value <0.10))
  g2<-delete.isolates(g2)
  # set seed to make the layout reproducible
  set.seed(3952)
  layout1 <- layout.fruchterman.reingold(g2)
  plot(g2, layout=layout1)
  
  #if we want to export to gephis plot tool
  library(rgexf) 
  wfmugraph<-igraph.to.gexf(g2)
  print(wfmugraf,file="wfmugraf.gexf")
  
}
#--------------------------------------------------------------------------------------------------
makeWordCloud<-function(djtdm=djtdm,maxWords=100) {
  #for wordcloud of most widely played artists
  #removing sparse terms at 0.99 means that artists played by fewer than 50 DJs will be dropped
  #and will return about 400 artists
  
  #just onMic?
  #idx <- meta(djCorpus, "cluster") == 5
  #djtdm<-TermDocumentMatrix(djCorpus[idx])%>%removeSparseTerms(0.80)
  
  
  m <- as.matrix(djtdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  t<-head(d, 200)
  rownames(t)<-NULL
  print(t)
  #save(t,file='artistfreq.txt',ascii = TRUE)
  
  print("Create Word Cloud")
  #scalefactor magnifies differences for wordcloud
  scaleFactor=3
  #maxWords = 200
  wordcloud(words = t$word, freq = t$freq^scaleFactor,max.words=maxWords, random.order=FALSE,rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"),scale = c(3,.3))
  
}
#------------------------------------------------------------------
#plot stuff depending on onmic or offmic status
plotStuff<-function(djtdm=djdtm,j=j,DJKey=DJkey){
  print(ggplot(DJKey[1:20,],aes(ShowName,artistCount))+geom_bar(stat="identity")+coord_flip())
  makeWordCloud(djtdm)
  assignClusters(j)
  #print(DJKey$ShowName)
}  
# --------------------------------------------------------------------------- MAIN --------------
load("allDJArtists.RData")

# allDJArtists<-cleanUpArtists(allDJArtists)
# 
# #combine first numWords words in artist name into a single token
# artistTokens<-combineArtistWords(allDJArtists,numWords=2)
# 
# 
# load("DJKey.RData")
# 
# DJKey<-addArtistCount(DJKey,artistTokens)
# #save(DJKey,file="DJKey.RData")
# 
# #get rid of DJs with less than 100 artists, ever.  Probably not a music show
# DJKey<-filter(DJKey,artistCount>100)
# artistTokens<-semi_join(artistTokens,DJKey)
# #regroup
# artistTokens<-artistTokens%>%group_by(DJ)
# 
# save(artistTokens,file="artistTokens.RData")
# load("artistTokens.RData")
# 
# 
# #combine words into one document per DJ
# # don't try to display djDocs. It's a monster and will hang machine!
# djDocs<-combineAllArtists()
# save(djDocs,file="djDocs.RData")
# load("djDocs.RData")
# 

#--------------------------------------------------------------------------------
# print("Create document corpus and term document matrices")
# djCorpus <- Corpus(VectorSource(djDocs$artists))
# 
# 
# for (i in 1:length(djCorpus)) {
#   meta(djCorpus[[i]], tag="ID") <- djDocs$DJ[i]  
#   meta(djCorpus[[i]], tag="id") <- djDocs$DJ[i]  
#   meta(djCorpus[[i]], tag="DJ") <- djDocs$DJ[i]
#   meta(djCorpus[[i]], tag="onMic") <- djDocs$onMic[i]
# }
# 
# 
# 
# micStatus=c("on","off","both")
# mic<-micStatus[2]
# #idx <- switch(mic,
# #              on = (meta(djCorpus, "onMic") == TRUE),
# #              off = (meta(djCorpus, "onMic") == FALSE),
# #              both = rep(TRUE,length(djCorpus))
# #              )
# 
# 
# #djtdm<-TermDocumentMatrix(djCorpus[idx])%>%removeSparseTerms(SPARSE)
# 
# #OR if you have the memory to pre-create
# #make 3 Term Document Matrices where artist is in the column based on onmic status
# 
# djtdm_all<-TermDocumentMatrix(djCorpus)%>%removeSparseTerms(SPARSE)
# djtdm_on<-TermDocumentMatrix(djCorpus[meta(djCorpus, "onMic") == TRUE])%>%removeSparseTerms(SPARSE)
# djtdm_off<-TermDocumentMatrix(djCorpus[meta(djCorpus, "onMic") == FALSE])%>%removeSparseTerms(SPARSE)
# 
# # now create Document Term Matrix where DJs are the column
# djdtm<-DocumentTermMatrix(djCorpus)%>%removeSparseTerms(SPARSE)
# 
# # get similarity index matrix using Jaccard
# j<-getSimilarity(djdtm)
# 
# onDJs<- DJKey%>%filter(onMic==TRUE)%>%select(DJ)%>%unlist()%>%as.vector()
# offDJs<-DJKey%>%filter(onMic==FALSE)%>%select(DJ)%>%unlist()%>%as.vector()
# AllDJs<-DJKey%>%select(DJ)%>%unlist()%>%as.vector()
#-----------------------------------------------------------------
#plot stuff
switch(mic,
       on = plotStuff(djtdm_on,j[onDJs,onDJs],DJKey[DJKey$DJ%in%onDJs,]),
       off = plotStuff(djtdm_off,j[offDJs,offDJs],DJKey[DJKey$DJ%in%offDJs,]),
       all = plotStuff(djtdm_all,DJKey)
              )


