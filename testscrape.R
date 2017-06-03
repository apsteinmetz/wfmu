testgetPlaylist <- function(plURLs,dj) {
  playlist = data.frame()
  i<-1
  print(paste(dj,i))

  #extract table data of class 'song'
  wholepage <- read_html(paste(ROOT_URL, plURLs[i],sep=''))
  plraw<-wholepage%>% html_nodes(css='.song')%>%
    html_text()%>%
    as.matrix()
  dim(plraw)<-c(5,length(plraw)/5)  
  plraw<-plraw%>%t()%>%as_tibble()
  names(plraw)<-plraw[1,]
  plraw<-plraw[-1,]
  plraw<-str_replace(plraw,'\n',"")
  plraw<-apply(plraw,c(1,2),str_replace_all,'\\n',"")
  plraw<-apply(plraw,c(1,2),str_trim)
  pl<-as_tibble(plraw)
  pl<-data.frame()
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
excludeDJs<-c('SD','HA','BC','AF','CP','HP','JP')
djList<-filter(DJKey,showCount>numShows-1,!(DJ%in%excludeDJs)) %>%select(DJ) %>% .[,1]
#djList<-filter(DJKey,showCount>numShows-1) %>%select(DJ) %>% .[,1]

testPL = list()
for (dj in djList) {
  plURLs<-playlistURLs %>% filter(DJ==dj) %>% select(playlistURL)%>%.[1:numShows,1] %>%as.character()
  testPL[[dj]]<-testgetPlaylist(plURLs,dj)
}
