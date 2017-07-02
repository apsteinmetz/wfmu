altArtistNames<-c('THE STOOGE','Band','Singer','Artist')
altTitleNames<-c('THE SONG','Track','Song')
altHeaderNames<-c(altArtistNames,altTitleNames)

fixHeaders<-function(pl){
  #takes a data frame
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
  print(paste(dj,i,plURLs[i,1]))

  wholepage <- read_html(paste(ROOT_URL, plURLs[i,1],sep=''))
  #try to pull out the show date.  assume first date in text on page is the show date
  airDate<-wholepage%>%
    html_text()%>%
    str_extract('[A-Za-z]+ [0-9]{1,2}, [0-9]{4}') %>% 
    as.Date("%B %d, %Y")
  if(is.na(airDate)) { #try something else
    airDate<-wholepage%>%
      html_text()%>%
      str_extract('[0-9]{1,2} [A-Za-z]+ [0-9]{4}') %>% 
      as.Date("%d %B %Y")
    
  }
  # are there rows with td of class=song?  get the table
  if((wholepage%>%html_nodes(xpath="//td[@class='song']")%>%length())>0) {
    plraw_head<-wholepage%>% 
      html_nodes(xpath="//th[@class='song']") %>% 
      html_text() %>%   
      str_replace('\n','') %>% 
      str_trim()
      
    plraw_row<-wholepage%>% 
      html_nodes(xpath="//td[@class='song']") %>% 
      html_nodes(xpath="//td[not(@colspan='7')]") #%>% 
      plraw<-plraw_row %>% 
      html_text() %>% 
      str_replace('\n','') %>% 
      str_trim() %>% 
      matrix(ncol=length(plraw_head),byrow=T) %>% 
      as_data_frame()

      names(plraw)<-plraw_head
    
    if (ncol(plraw)>10) plraw<-handleBadTable(wholepage)
  } else  {
    #if not, does it have a table row with "artist | title | song | album".  assume first such table is playlist
    if(wholepage%>%html_node(xpath="//tr[td='Artist'] or //tr[td='Album']")) {
      plraw<-wholepage%>% 
        html_node(xpath="//tr[td='Artist']/ancestor::table") %>%
        html_table(fill=TRUE)
    } else {
      plraw<-NULL #we got nothin'
    }
  }
  if(is.null(plraw)) {
    playlist<-plraw
    print('DUD')
    } else{ # try one more thing
      if (names(plraw)[1]=="X1"){
        #scan until we find the playlist header
        for (n in 1:nrow(plraw)){
          if (plraw$X1[n]%in%altHeaderNames){
            names(plraw)<-plraw[n,]
            plraw<-plraw[n+1:nrow(plraw),]
            break
          }
          
          
        }
      }
      if(TRUE%in%is.na(names(plraw))) plraw<-plraw[,-which(is.na(names(plraw)))] #sometimes an NA column
      
      playlist<-plraw%>%
        as_data_frame()%>%
        fixHeaders() %>%
        mutate(DJ=dj,AirDate=airDate)%>%
        select(DJ,AirDate,Artist,Title)%>%
        filter(Artist!='')
      print(playlist[1:5,])
    }

  return(playlist)
}



#-------------- MAIN -----------------
#limit analysis to DJs with at least numShows shows and take the last numshows shows.
numShows<-50
# non-music shows
excludeDJs<-c('SD','HA','BC','AF','CP','HP','JP','GM','DC','CC','DU','ES')
djList<-filter(DJKey,showCount>numShows-1,!(DJ%in%excludeDJs)) %>% 
  select(DJ) %>% .[,1]
#djList<-filter(DJKey,showCount>numShows-1) %>%select(DJ) %>% .[,1]

testPL = data_frame()
for (dj in djList) {
  plURLs<-playlistURLs %>% 
    filter(DJ==dj) %>%
    .[1:numShows,] %>% 
    select(playlistURL) 
  testPL<-bind_rows(testPL,testgetPlaylist(plURLs,dj))
}
