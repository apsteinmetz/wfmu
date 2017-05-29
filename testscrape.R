testgetPlaylist <- function(plURLs,dj) {
  playlist = data.frame()
  i<-1
  print(paste(dj,i))
  #assume playlist is a table.  scan all tables on page to find which is the correct one
  # select th is assumed to be unique to playlist tables.  not proven.
  alltables <- read_html(paste(ROOT_URL, plURLs[i],sep=''))%>%html_nodes("table")
  pl<-data.frame()
  for (n in 1:length(alltables)) {
    if (length(html_nodes(alltables[n],'th')) > 0) {
      pl<-alltables[[n]]%>%html_table(fill=TRUE)
      } #we found the table with playlists
  }
  

  #temp<-html(plURL[i])%>%html_nodes("table")%>%.[2]%>%html_table(fill=TRUE)
  # try to correct tables without headers
  #headers might be in second row
  if (is.null(names(pl)) || names(pl)[1]=="X1") {
    names(pl)<- temp[1,]
    pl <- pl[-1,]
    
  }
  #exceptions we are aware of where table is nice but headers are not labeled "artist" ,"title"
  altTitleNames<-c('THE SONG','Track')
  altArtistNames<-c('THE STOOGE')
  names(temp)[names(temp)%in%altTitleNames]<-"Title"
  names(temp)[names(temp)%in%altArtistNames]<-"Artist"
  # order of artist, track(or title) varies.   Fix it
  if (dim(temp)[2]>1) {  #ignore single column playlists  
      playlist<-rbind(playlist,cbind(dj,Artist=temp["Artist"],Title=temp['Title']))
      
    }
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
