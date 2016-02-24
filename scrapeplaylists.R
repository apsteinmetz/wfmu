# # install.packages("devtools")
# # require(devtools)
# # install_url("http://www.omegahat.org/Rstem/Rstem_0.4-1.tar.gz")
# # install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
# # install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
# 
library(rvest)
# library(stringi)
# library(tm)
# library("plyr")
# library("ggplot2")
# library("wordcloud")
# library("RColorBrewer")
# library("SnowballC")
# library(sentiment)
# library("stringr")
library(xml2)
# library(data.table)
# 
# 


#actual work. testing above
# ----------------------------------------------
allDJURLs <-"http://wfmu.org/playlists"
playListRaw<- html(allDJURLs)

# get the urls of the each DJs RSS playlist feed
t<-html_nodes(playListRaw,"ul")[2] %>% html_nodes(xpath='//a[contains(.,"Playlists")]')  %>% html_attr(name="href") 
DJURLs<-paste("http://wfmu.org",t,sep="")[-1]
#--------------------------------------------------------------------------
#get all playlists for a DJ
# TROUBLE all play list tables are not the same. Headers might not match
getPlaylist <- function(plURL,dj) {
  playlist = data.frame()
  for (i in 1:length(plURL)) {
    print(paste(dj,i))
    # first two columns contain artist and track name, leave the rest
    temp <- (html(plURL[i])%>%html_node(xpath="//table[2]")%>%html_table(fill=TRUE))
    # try to correct tables without headers
    if (names(temp)[1]=="X1") {
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
allDJPlayLists = data.frame()
DJKey = data.frame()
for (n in 1:length(DJURLs)) {
  singleDJ<- html(DJURLs[n])
  playlistURL<-html_nodes(singleDJ,xpath = "//guid") %>%html_text()
  showName <- html_node(singleDJ,"title")%>%html_text()
  showName <- sub("WFMU's recent playlists from ","",showName)
  DJ <- sub(".xml","",sub("http://wfmu.org/playlistfeed/","",DJURLs[n]))
  DJKey<-rbind(DJKey,data.frame(DJ=DJ,ShowName=showName))
  allDJPlayLists = rbind(allDJPlayLists, getPlaylist(plURL = playlistURL,dj = DJ))
}  

