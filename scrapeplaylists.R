# scrape playlists

library(rvest)
library(stringr)
library(xml2)
library(tidyverse)



# ----------------------------------------------
ROOT_URL<-"http://wfmu.org"
LAST_DJ<-"CI" #last currently on-mic DJ in DJ list  Need to smartly detect this

#-------------------------------------------
getDJURLs <- function(){
  rawDJURLs<- read_html(paste(ROOT_URL,"/playlists",sep=""))
  # get the urls of the each DJs RSS playlist feed
  t<-rawDJURLs%>%html_nodes(xpath='//html//body//center[2]//table[1]//table//a[contains(.,"Playlists")]') %>% 
    html_attr(name="href")

  DJURLs<-paste("http://wfmu.org",t,sep="")[-1]
  # above got the RSS feed links but we want the longer list of shows.  Below modifies
  # the URL to get the right link
  DJURLs<- gsub("playlistfeed","playlists",DJURLs)
  DJURLs<- gsub(".xml","",DJURLs)
  
  return(DJURLs)
}
#--------------------------------------------------------------------------
#-------------------------------------------
getDJsOffSched <- function(){
  #table 9 is off sched. 2-8 are monday through sunday
  rawDJURLs<- read_html(paste(ROOT_URL,"/playlists",sep=""))
  t_off<-rawDJURLs%>%html_nodes(xpath='//html//body//center[2]//table[1]//table[9]//a[contains(.,"Playlists")]')  %>% html_attr(name="href") 
  d_off <- str_replace(t_off,"/playlistfeed/","")
  d_off <- str_replace(d_off,".xml","")
  return(d_off)
}

# #---------------------------------------------------
# get the shownames for a DJ
getShowNames<-function(DJURLs) {
  DJKey = data.frame()
  for (page in DJURLs) {
    singleDJ<- read_html(page)
    showName <- html_node(singleDJ,"title")%>%html_text()
    showName <- gsub("\n","",sub("Playlists and Archives for ","",showName))
    showName<-str_replace(showName,'WFMU:',"")
    showName<-str_replace_all(showName,':Playlists and Archives',"")
    DJ <- sub("http://wfmu.org/playlists/","",page)
    DJKey<-rbind(DJKey,data.frame(DJ=DJ,ShowName=showName))
    print(showName)
  }
  # now identifty those DJs which are currently ON MIC
  DJKey$onSched <- TRUE
  DJKey$onSched[which(DJKey$DJ %in% getDJsOffSched())]<-FALSE
  #strip "WFMU" and "Playlists and Archives" and some punctuation
  DJKey$ShowName<-str_replace_all(DJKey$ShowName,"(P|p)laylists (and|&) (A|a)rchives","")
  DJKey$ShowName<-str_replace_all(DJKey$ShowName,"-","")
  DJKey$ShowName<-str_replace_all(DJKey$ShowName,"(P|p)laylist|(R|r)ecent","")
  DJKey$ShowName<-str_replace_all(DJKey$ShowName,"WFMU|wfmu","")
  DJKey$ShowName<-str_trim(DJKey$ShowName)
  

  return (DJKey)  
  #save(DJKey,file="DJKey.RData")
}

# #---------------------------------------------------
# get the URLs of the playlists for a DJ
getDJPlaylistURLs<-function(DJURLs) {
  allDJPlayLists = data.frame(DJ=NULL,playlistURL=NULL)
  dudList<-NULL
  #DJKey = data.frame()
  for (n in 1:length(DJURLs)) {
    singleDJ<- read_html(DJURLs[n])
    pl<-singleDJ%>%
      html_node(xpath="//div[@class='showlist']")%>%
      html_nodes(xpath="//a")%>%
      html_attr("href")
    #format for newer shows
    pl1<-as.character(na.omit(pl[str_detect(pl,"playlists/shows")]))
    # format for older shows
    pl2<-as.character(na.omit(pl[str_detect(pl,"Playlist")]))
    playlistURL<-c(pl1,pl2)
    playlistURL<-str_replace_all(playlistURL,'http://wfmu.org','')%>%as.character()

    showdates<-singleDJ%>%
      html_node(xpath="//div[@class='showlist']")%>%
      # html_nodes(xpath="//span") %>% 
      html_text()%>%
      str_extract_all('[A-Za-z]+ [0-9]{1,2}, [0-9]{4}') #%>%
      strptime("%B %d, %Y")
    #showName <- html_node(singleDJ,"title")%>%html_text()
    #showName <- gsub("\n","",sub("Playlists and Archives for ","",showName))
    DJ <- sub("http://wfmu.org/playlists/","",DJURLs[n])
    print(DJ)
    #DJKey<-rbind(DJKey,data.frame(DJ=DJ,ShowName=showName))
    #omit shows without valid playlists.  Talk shows?
    if (length(playlistURL)>0) {
      allDJPlayLists = rbind(allDJPlayLists, data.frame(DJ=DJ,playlistURL = playlistURL))
    } else { 
      print("DUD")
      dudList<-c(dudList,DJ) }
  }  
  
  return(allDJPlayLists)
}

#-------------------------------------------------
# Get all Artists ever played by a DJ
#WFMU maintains this as a separate page
getDJArtistNames<-function(DJURLs) {
  # scrape artist names for all DJs from the link at the bottom of each DJ page
  allDJArtists<-data.frame()
  URL_BRANCH<- "/artistkeywords.php/"
  for (page in DJURLs) {
    singleDJ<- read_html(page)
    showName <- html_node(singleDJ,"title")%>%html_text()
    showName <- gsub("\n","",sub("Playlists and Archives for ","",showName))
    DJ <- sub("http://wfmu.org/playlists/","",page)
    DJKey<-rbind(DJKey,data.frame(DJ=DJ,ShowName=showName))
    print(showName)
    artistListPage <- paste(ROOT_URL,URL_BRANCH,DJ, sep="")
    artistList<-read_html(artistListPage)%>%html_node(xpath="//body/div")%>%html_text()%>%str_split("\n")
    DJArtists<-data.frame(DJ,artistRaw=unlist(artistList))
    if (nrow(DJArtists) >0) allDJArtists = rbind(allDJArtists,DJArtists)
    #remove factor level of DJs with no artists
    save(allDJArtists,file="allDJArtists.RData")
  }
  return(allDJArtists)
}  

#--------------------------------------------------------------------------
#get all playlists for a DJ
# TROUBLE all play list tables are not the same. Headers might not match
getPlaylist <- function(plURLs,dj) {
  playlist = data.frame()
  for (i in 1:length(plURLs)) {
    print(paste(dj,i))
    # first two columns contain artist and track name, leave the rest
    temp <- read_html(paste(ROOT_URL, plURLs[i],sep=''))%>%html_node(xpath="//table[2]")%>%html_table(fill=TRUE)
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


# just get first playlist to test parsing. TROUBLE all play list tables are not the same. Headers might not match
testgetPlaylist <- function(plURLs,dj) {
    playlist = data.frame()
    i<-1
    print(paste(dj,i))
    # first two columns contain artist and track name, leave the rest
    temp <- read_html(paste(ROOT_URL, plURLs[i],sep=''))%>%html_node(xpath="//table[2]")%>%html_table(fill=TRUE)
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
  return(playlist[1:10,])
}



#-------------- MAIN -----------------
DJURLs<-getDJURLs()
DJKey<-getShowNames(DJURLs)
#load(file='djkey.rdata')
playlistURLS<-getDJPlaylistURLs(DJURLs)
showCounts<-playlistURLs %>% group_by(DJ) %>% summarise(showCount=n()) %>% arrange(desc(showCount))
DJKey<-left_join(DJKey,showCounts)
#limit analysis to DJs with at least numShows shows and take the last numshows shows.
numShows<-50
djList<-filter(DJKey,showCount>numShows-1) %>%select(DJ)
for (dj in djList) {
  playlistURLs %>% filter(DJ==dj) %>% select(playlistURL)%>%.[1:numShows,1] %>%as.character() %>% getPlaylist(dj)
}

testPL = list()
for (dj in djList) {
  plURLs<-playlistURLs %>% filter(DJ==dj) %>% select(playlistURL)%>%.[1:numShows,1] %>%as.character()
  testPL[[dj]]<-testgetPlaylist(plURLs,dj)
}
#allDJArtists <- getDJArtistNames(DJURLs) 
#allDJArtists <- filter(allDJArtists,artist!="")
# artists are a factor by default. change it to character
#allDJArtists$artistRaw<-as.character(allDJArtists$artistRaw)
#save(allDJArtists,file="allDJArtists.RData")
