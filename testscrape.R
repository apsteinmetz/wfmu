altArtistNames<-c('THE STOOGE','Band','Singer','Artist')
altTitleNames<-c('THE SONG','Track','Song')
altHeaderNames<-c(altArtistNames,altTitleNames)
header_text_xpath<-paste("//tr[th='Track']",
                          "or //tr[th='Title']",
                          "or //tr[contains(th,'Song')]")
                         "or //tr[contains(th,'SONG')]")
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
  
  plraw<- NULL
  #simplest case. A table with obvious header names
  #hope we have all variations on the song title label
  if(wholepage%>%html_node(xpath=header_text_xpath)) {
    plraw<-wholepage%>% 
      html_node(xpath="//td[@class='song']/ancestor::table") %>%
      html_table(fill=TRUE)
  } else {
    # are there rows in a table with td of class=song?  get the table
    if(wholepage%>%html_node(xpath="//td[@class='song']") %>% length()>1) {
      plraw<-wholepage %>% 
        html_node(xpath="//td[@class='song']/ancestor::table") %>% 
        html_table(fill=T)
      #now find the row that has the header
      for (n in 1:nrow(plraw)){
        if (TRUE %in% (plraw[n,]%in%altHeaderNames)){
          names(plraw)<-plraw[n,]
          plraw<-plraw[n+1:nrow(plraw),]
          break
        }
        if (n==nrow(plraw)) {
          print("DUD. Can't find header")
          plraw<-NULL #final dead end
        }
        
      }
    }
  }
  
  if (is.null(plraw)){
    # not an html table. now what? try to make a table after class song and finding header
    if((wholepage%>%html_nodes(xpath="//td[@class='song']")%>%length())>0) {
      plraw_head<-wholepage%>% 
        html_nodes(xpath="//td[(@class='song') and not(@colspan | @align)]") %>% 
        html_text() %>%   
        str_replace('\n','') %>% 
        str_trim()

        plraw_row<-wholepage %>% 
        html_nodes(xpath="//td[(@class='song') and not(@colspan | @align)]") %>% 
        html_text() %>% 
        str_replace('\n','') %>% 
        str_trim() %>% 
        matrix(ncol=length(plraw_head),byrow=T) %>% 
        as_data_frame()
      
      names(plraw)<-plraw_head
      plraw<-NULL #we got nothin'
    }
  }
  if(is.null(plraw)) {
    print("DUD. Can't find a playlist.")
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
      if (n==nrow(plraw)) {
        print("DUD. Can't find header")
        plraw<-NULL #final dead end
      }
    }
  }
# final clean up if we have something
if (is.null(plraw)){ playlist<- NULL
} else {
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
