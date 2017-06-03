testgetPlaylist <- function(plURLs,dj) {
  playlist = data.frame()
  i<-1
  print(paste(dj,i))
  #assume playlist is a table.  scan all tables on page to find which is the correct one
  
  wholepage <- read_html(paste(ROOT_URL, plURLs[i],sep=''))
  plraw<-wholepage%>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "song", " " ))]')%>%
    html_text()%>%
    as.matrix()
  dim(plraw)<-c(5,length(plraw)/5)  
  plraw<-plraw%>%t()%>%as.data.frame()
  names(plraw)<-plraw[1,]
  
  pl<-data.frame()
  for (n in 1:length(alltablerows)) {
    if (length(html_nodes(alltables[n],'th')) > 0) {
      pl<-alltables[[n]]%>%html_table(fill=TRUE)
      } #we found the table with playlists
  }
  #TABLE STYLE 2
  if (length(pl)==0){
    pl<-alltables[[3]]%>%html_table(fill=T)
    if (is.null(names(pl)) || names(pl)[1]=="X1") {
      names(pl)<- pl[1,]
      pl <- pl[-1,]
    }
  }  
  #temp<-html(plURL[i])%>%html_nodes("table")%>%.[2]%>%html_table(fill=TRUE)
  # try to correct tables without headers
  #headers might be in second row
  if (is.null(names(pl)) || names(pl)[1]=="X1") {
    names(pl)<- pl[1,]
    pl <- pl[-1,]
    
  }
  #exceptions we are aware of where table is nice but headers are not labeled "artist" ,"title"
  altTitleNames<-c('THE SONG','Track')
  altArtistNames<-c('THE STOOGE')
  names(pl)[names(pl)%in%altTitleNames]<-"Title"
  names(pl)[names(pl)%in%altArtistNames]<-"Artist"
  # order of artist, track(or title) varies.   Fix it
  if (dim(pl)[2]>1) {  #ignore single column playlists  
      playlist<-rbind(playlist,cbind(dj,Artist=pl["Artist"],Title=pl['Title']))
      
  }
  playlist<-filter(playlist,Artist!='')
  return(playlist[1:10,])
}



#-------------- MAIN -----------------
#limit analysis to DJs with at least numShows shows and take the last numshows shows.
numShows<-50
# non-music shows
excludeDJs<-c('SD','HA','BC')
djList<-filter(DJKey,showCount>numShows-1,!(DJ%in%excludeDJs)) %>%select(DJ) %>% .[,1]
#djList<-filter(DJKey,showCount>numShows-1) %>%select(DJ) %>% .[,1]

testPL = list()
for (dj in djList) {
  plURLs<-playlistURLs %>% filter(DJ==dj) %>% select(playlistURL)%>%.[1:numShows,1] %>%as.character()
  testPL[[dj]]<-testgetPlaylist(plURLs,dj)
}
