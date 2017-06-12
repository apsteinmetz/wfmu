fixHeaders<-function(pl){
  #takes a data fram
  altTitleNames<-c('THE SONG','Track')
  altArtistNames<-c('THE STOOGE')
  names(pl)[names(pl)%in%altTitleNames]<-"Title"
  names(pl)[names(pl)%in%altArtistNames]<-"Artist"
  return(pl)
}

handleBadTable<-function(wholepage){
  headers<-wholepage%>% 
    html_node(xpath="//th[@class='song']/parent::tr")%>%
    html_text()%>%
    str_split('\n') %>% 
    unlist()
  plraw<-wholepage%>% 
    html_nodes(xpath="//td[@class='song']/parent::tr")%>%
    html_text()%>%
    str_split('\n') %>% 
    as.data.frame() %>% 
    t() %>% 
    as_data_frame()
  plraw<-plraw[,!sapply(plraw,function(x) (all(x==""))),drop=F]
 # still has columns with only on-printable characters

  #NOT WORKING

}
  
#--------------------------------------------------------------------
testgetPlaylist <- function(plURLs,dj) {
  i<-1
  print(paste(dj,i,plURLs[i]))

  wholepage <- read_html(paste(ROOT_URL, plURLs[i],sep=''))
  #try to pull out the show date.  assume first date in text on page is the show date
  airDate<-wholepage%>%
    html_text()%>%
    str_extract('[A-Za-z]+ [0-9]{1,2}, [0-9]{4}') %>% 
    as.Date("%B %d, %Y")
  # are there rows with td of class=song?  get the table
  if((wholepage%>%html_node(xpath="//td[@class='song']")%>%length())>0) {
    plraw<-wholepage%>% 
      html_node(xpath="//td[@class='song']/ancestor::table")%>%
      html_table(fill=TRUE) 
    if (ncol(plraw)>10) plraw<-handleBadTable(wholepage)
  } else  {
    #if not, does it have a table row with "artist | title | song | album".  assume first such table is playlist
    if(wholepage%>%html_node(xpath="//tr[td='Artist'] or //tr[td='Album']")) {
      plraw<-wholepage%>% 
        html_node(xpath="//tr[td='Artist']/ancestor::table") %>%
        html_table(fill=TRUE)
    } else {
      print('fail') #placeholder
    }
  }
  if (names(plraw)[1]=="X1"){
    names(plraw)<-plraw[1,]
    plraw<-plraw[-1,]
  }
  if(TRUE%in%is.na(names(plraw))) plraw<-plraw[,-which(is.na(names(plraw)))] #sometimes an NA column
  
  playlist<-plraw%>%
    as_data_frame()%>%
    fixHeaders() %>%
    mutate(DJ=dj,AirDate=airDate)%>%
    select(DJ,AirDate,Artist,Title)%>%
    filter(Artist!='')
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
  print(playlist[1:5,])
  return(playlist)
}



#-------------- MAIN -----------------
#limit analysis to DJs with at least numShows shows and take the last numshows shows.
numShows<-50
# non-music shows
excludeDJs<-c('SD','HA','BC','AF','CP','HP','JP','GM')
djList<-filter(DJKey,showCount>numShows-1,!(DJ%in%excludeDJs)) %>%select(DJ) %>% .[,1]
#djList<-filter(DJKey,showCount>numShows-1) %>%select(DJ) %>% .[,1]

testPL = list()
for (dj in djList) {
  plURLs<-playlistURLs %>% filter(DJ==dj) %>% select(playlistURL)%>%.[1:numShows,1] %>%as.character()
  testPL[[dj]]<-testgetPlaylist(plURLs,dj)
}
