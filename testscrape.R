fixHeaders<-function(pl){
  #takes a data fram
  altTitleNames<-c('THE SONG','Track')
  altArtistNames<-c('THE STOOGE')
  names(pl)[names(pl)%in%altTitleNames]<-"Title"
  names(pl)[names(pl)%in%altArtistNames]<-"Artist"
  return(pl)
}

testgetPlaylist <- function(plURLs,dj) {
  playlist = data.frame()
  i<-1
  print(paste(dj,i))

  wholepage <- read_html(paste(ROOT_URL, plURLs[i],sep=''))
  # are there rows with td of class=song?  get the table
  plraw<-wholepage%>% 
    html_node(xpath="//td[@class='song']/ancestor::table")%>%
    html_table(fill=TRUE)%>%
    select(-`NA`) %>% #sometimes an NA column
    as_data_frame()
    
  #if not, does a table have a border element.  assume first such table is playlist
  if (length(plraw)==0){
    print('fail') #placeholder
  } else{
    playlist<-plraw%>%
      fixHeaders() %>%
      mutate(DJ=dj)%>%
      select(DJ,Artist,Title)%>%
      filter(Artist!='')
  }
  #if not?
  #is there a th row?  use it as header
  # plraw<-wholepage%>% html_nodes(xpath="//td[class=song")%>%
  #   html_text()%>%
  #   as.matrix()
  # dim(plraw)<-c(5,length(plraw)/5)  
  # plraw<-plraw%>%t()%>%as_tibble()
  # names(plraw)<-plraw[1,]
  # plraw<-plraw[-1,]
  # plraw<-str_replace(plraw,'\n',"")
  # plraw<-apply(plraw,c(1,2),str_replace_all,'\\n',"")
  # plraw<-apply(plraw,c(1,2),str_trim)
  # pl<-as_tibble(plraw)
  #exceptions we are aware of where table is nice but headers are not labeled "artist" ,"title"
  # order of artist, track(or title) varies.   Fix it
  # 
  #   if (dim(pl)[2]>1) {  #ignore single column playlists  
  #     playlist<-rbind(playlist,cbind(dj,Artist=pl["Artist"],Title=pl['Title']))
  #     
  # }
  # playlist<-filter(playlist,Artist!='')
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
