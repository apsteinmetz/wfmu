# # install.packages("devtools")
# # require(devtools)
# # install_url("http://www.omegahat.org/Rstem/Rstem_0.4-1.tar.gz")
# # install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
# # install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
# 
# library(stringi)
# library(tm)
# library("plyr")
# library("ggplot2")
# library("wordcloud")
# library("RColorBrewer")
# library("SnowballC")
# library(sentiment)library(rvest)

library(rvest)
library(stringr)
library(XML)
library(xml2)
library(dplyr)
# library(data.table)
# 
# 


#actual work. testing above
# ----------------------------------------------
allDJURLs <-"http://wfmu.org/playlists"
ROOT_URL<-"http://wfmu.org"
playListRaw<- html(allDJURLs)

# get the urls of the each DJs RSS playlist feed
t<-html_nodes(playListRaw,"ul")[2] %>% html_nodes(xpath='//a[contains(.,"Playlists")]')  %>% html_attr(name="href") 
DJURLs<-paste("http://wfmu.org",t,sep="")[-1]
# above got the RSS feed links but we want the longer list of shows.  Below modifies
# the URL to get the right link
DJURLs<- gsub("playlistfeed","playlists",DJURLs)
DJURLs<- gsub(".xml","",DJURLs)

#--------------------------------------------------------------------------
#get all playlists for a DJ
# TROUBLE all play list tables are not the same. Headers might not match
getPlaylist <- function(plURL,dj) {
  playlist = data.frame()
  for (i in 1:length(plURL)) {
    print(paste(dj,i))
    # first two columns contain artist and track name, leave the rest
    temp <- html(plURL[i])%>%html_node(xpath="//table[2]")%>%html_table(fill=TRUE)
    #temp<-html(plURL[i])%>%html_nodes("table")%>%.[2]%>%html_table(fill=TRUE)
    # try to correct tables without headers
    if (is.null(names(temp)) || names(temp)[1]=="X1") {
      names(temp)<- temp[1,]
      temp <- temp[-1,]
      
    }
    # order of artist, track(or title) varies.   Fix it
    if (dim(temp)[2]>1) {  #ignore single column playlists  
      if (names(temp)[1]=="Artist") {
        playlist<-rbind(playlist,cbind(dj,temp["Artist"],Track=temp[,2]))
     }
      else {
       playlist<-rbind(playlist,cbind(dj,temp["Artist"],Track=temp[,1]))
      
      }
    }
  }
  return(playlist)
}

# #---------------------------------------------------
# get the URLs of the playlists for a DJ
getDJPlaylistURLs<-function() {
  allDJPlayLists = data.frame()
  DJKey = data.frame()
  for (n in 1:length(DJURLs)) {
    singleDJ<- html(DJURLs[n])
    pl<-html_nodes(singleDJ,xpath="//a")%>%html_attr("href")
    #format for newer shows
    pl1<-paste(ROOT_URL, na.omit(pl[str_detect(pl,"playlists/shows")]),sep="")
    # format for older shows
    pl2<-paste(ROOT_URL, na.omit(pl[str_detect(pl,"Playlist")]),sep="")
    playlistURL<-c(pl1,pl2)
    showName <- html_node(singleDJ,"title")%>%html_text()
    showName <- gsub("\n","",sub("Playlists and Archives for ","",showName))
    DJ <- sub("http://wfmu.org/playlists/","",DJURLs[n])
    DJKey<-rbind(DJKey,data.frame(DJ=DJ,ShowName=showName))
    allDJPlayLists = rbind(allDJPlayLists, getPlaylist(plURL = playlistURL,dj = DJ))
  }  
}

getDJArtistNames<-function(DJURLs)
  URL_BRANCH<- "/artistkeywords.php/"
  for (page in DJURLs) {
    singleDJ<- html(page)
    showName <- html_node(singleDJ,"title")%>%html_text()
    showName <- gsub("\n","",sub("Playlists and Archives for ","",showName))
    DJ <- sub("http://wfmu.org/playlists/","",page)
    artistListPage <- paste(ROOT_URL,URL_BRANCH,DJ, sep="")
    artistList<-html(artistListPage) #%>%html_text()
    #DJKey<-rbind(DJKey,data.frame(DJ=DJ,ShowName=showName))
    #allDJPlayLists = rbind(allDJPlayLists, getPlaylist(plURL = playlistURL,dj = DJ))
  }  


  
