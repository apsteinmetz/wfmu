# scrape playlists

library(rvest)
library(stringr)
library(xml2)
library(tidyverse)





# ----------------------------------------------
ROOT_URL<-"http://wfmu.org"

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
#-------------------------------------------
getDJsOffSched <- function(){
  #table 9 is off sched. 2-8 are monday through sunday
  rawDJURLs<- read_html(paste(ROOT_URL,"/playlists",sep=""))
  t_off<-rawDJURLs%>%html_nodes(xpath='//html//body//center[2]//table[1]//table[9]//a[contains(.,"Playlists")]')  %>% html_attr(name="href") 
  d_off <- str_replace(t_off,"/playlistfeed/","")
  d_off <- str_replace(d_off,".xml","")
  return(d_off)
}

#---------------------------------------------------
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
getDJPlaylistURLs<-function(music_djs) {
  allDJPlayLists = NULL
  dudList<-NULL
  #DJKey = data.frame()
  for (dj in music_djs) {
    singleDJ<- read_html(paste("http://wfmu.org/playlists/",dj,sep=''))
    #singleDJ<- read_html("http://wfmu.org/playlists/HN")
    pl<-singleDJ%>%
        html_nodes(xpath="//a[contains(@href,'playlists/shows')]") %>%
        html_attr("href")
    #format for newer shows
    pl<-as.character(na.omit(pl[str_detect(pl,"playlists/shows")]))
    # format for older shows
    if (length(pl)<1) pl<-as.character(na.omit(pl[str_detect(pl,"Playlist")]))
    
    #assume a full URL is a fill-in DJ.  We omit these from the analysis
    pl<-pl[!str_detect(pl,"http")]
    
    playlistURL<-pl %>% as.character()
    print(dj)
    #omit shows without valid playlists.  Talk shows?
    if (length(playlistURL)>0) {
      allDJPlayLists = bind_rows(allDJPlayLists, data_frame(DJ=dj,playlistURL = playlistURL))
    } else { 
      print("DUD")
      dudList<-c(dudList,dj) }
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
altArtistNames <- c('THE STOOGE', 'Band', 'Singer', 'Artist')
altTitleNames <- c('THE SONG', 'Track', 'Song','Title')
altHeaderNames <- c(altArtistNames, altTitleNames)
MAXLEN = 60 #how long should we let artist and title names be. Truncate longer
header_th_xpath <- paste(
  "//th='Track'",
  "or //th='Title'",
  "or //th='Song'",
  "or //th='THE SONG'"
)
header_td_xpath <- paste(
  "//td='Track'",
  "or //td='Title'",
  "or //td='Song'",
  "or //td='THE SONG'",
  "or //td='Artist'",
  "or //td='THE SONG'",
  "or //td='THE STOOGE'"
)


#------------------------------------------------------------------
try_BK<-function(wp){
  #assume first field is title and second is artist separated by dash
  #doing it it two steps assure same number of artists and titles
  title_artist<-wp %>% 
    html_nodes(xpath="//table[2]") %>% 
    html_text() %>% 
    str_extract_all("\n[\\S ]+\n-\n[\\S ]+\n") %>% 
    .[[1]] 
  
  
  Title<-title_artist %>% 
    str_extract_all("\n[\\S ]+\n-") %>% 
    str_replace_all("\n(-)?","")
  
  Artist<-title_artist %>% 
    str_extract_all("\n-\n[\\S ]+\n") %>% 
    str_replace_all("\n(-)?","")
  plraw<-tibble(Artist,Title)
  
}
#------------------------------------------------------------------
try_HN<-function(wp){
  #assume first field is title and second is artist separated by colon
  #doing it it two steps assure same number of artists and titles
  title_artist<-wp %>% 
    html_nodes(xpath="//table[2]") %>% 
    html_text() %>% 
    str_extract_all("\n[\\S ]+: [\\S ]+\n") %>% 
    .[[1]] 
  
  
  Artist<-title_artist %>% 
    str_extract_all("\n[\\S ]+: ") %>% 
    str_replace_all("(\n)|(: )","")
  
  Title<-title_artist %>% 
    str_extract_all(": [\\S ]+\n") %>% 
    str_replace_all("(\n)|(: )","")
  plraw<-tibble(Artist,Title)
  
}#------------------------------------------------------------------

fixHeaders <- function(pl) {
  #takes a data frame
  nm<-which(names(pl) %in% altArtistNames)
  tt<-which(names(pl) %in% altTitleNames)
  if (length(nm)>0){
    names(pl)[nm] <- "Artist"
  } else{
    pl=NULL
    return(pl)
  } 
  if (length(tt)>0){
    names(pl)[tt] <- "Title"
  } else{
    pl$Title=NA
  }
  
  return(pl)
}

#--------------------------------------------------------------------
testgetPlaylist <- function(plURL, dj) {
  
  wholepage <- read_html(paste(ROOT_URL, plURL, sep = ''))
  #try to pull out the show date.  assume first date in text on page is the show date
  airDate <- wholepage %>%
    html_text() %>%
    str_extract('[A-Za-z]+ [0-9]{1,2}, [0-9]{4}') %>%
    as.Date("%B %d, %Y")
  if (is.na(airDate)) {
    #try something else
    airDate <- wholepage %>%
      html_text() %>%
      str_extract('[0-9]{1,2} [A-Za-z]+ [0-9]{4}') %>%
      as.Date("%d %B %Y")
    
  }
  
  plraw <- NULL
  #hand-rolled
  #simplest case. A table with obvious header names
  if (!is.na(wholepage %>% html_node(xpath = "//th[@class='song']"))) {
    table_shell<-xml_new_root("table")
    #remove single column rows, I hope nothing else.
    wholepage %>% html_nodes(xpath="//td[@colspan='8']") %>% xml_remove(free=T)
    plraw<-wholepage%>% 
      html_nodes(xpa="//tr[td[@class ='song']] | //tr[th[@class ='song']]")
    for (node in plraw)  xml_add_child(table_shell,node)
    plraw<-table_shell %>% 
      html_node(xpath="//table") %>% 
      html_table(fill=TRUE)
    
  } else {
    # no 'th' but are there rows in a table with td of class=song?  get the table
    if (!is.na(wholepage %>% html_node(xpath = "//td[@class='song']"))) {
      plraw <- wholepage %>%
        html_node(xpath = "//td[@class='song']/ancestor::table") %>%
        html_table(fill = T)
      #now find the row that has the header
      for (n in 1:nrow(plraw)) {
        if (TRUE %in% (plraw[n, ] %in% altHeaderNames)) {
          names(plraw) <- plraw[n, ]
          plraw <- plraw[n + 1:nrow(plraw), ]
          break
        }
        if (n == nrow(plraw)) {
          print("DUD. Can't find header")
          plraw <- NULL
        }
      }
    }
  }
  
  if (is.null(plraw)) {
    # no song class, now what? is it a table? try to  find header
    #seems like cellspacing means its a row column thing
    pl_table<-wholepage %>% html_node(xpath = "//table[@cellspacing and @cellpadding]")
    if (length(pl_table)>2) {
      pl_table<-html_table(pl_table,fill = TRUE)
      if (any(names(pl_table) %in% altHeaderNames)) {
        plraw<-pl_table
      } else{
        # try one more ``
        #scan until we find the playlist header
        for (n in 1:nrow(pl_table)) {
          if (TRUE %in% (pl_table[n,] %in% altHeaderNames)) {
            names(pl_table) <- pl_table[n, ]
            plraw <- pl_table[n + 1:nrow(pl_table), ]
            break
          }
        }
        if (n == nrow(pl_table)) {
          plraw<-NULL
          #try idiosyncratic djs
          if (dj=="TW") plraw<-try_BK(wholepage)
          if (dj=="HN") plraw<-try_HN(wholepage)
          if (is.null(plraw)) print("DUD. Can't find header")
          #final dead end
        }
      }
    }
  }
  plraw<-fixHeaders(plraw)
  # final clean up if we have something
  if (is.null(plraw)) {
    playlist <- NULL
  } else {
    if (TRUE %in% is.na(names(plraw)))
      plraw <- plraw[, -which(is.na(names(plraw)))] #sometimes an NA column
    
    playlist <- plraw %>%
      select(Artist,Title) %>% 
      na.omit() %>% 
      transmute(DJ = dj, 
                AirDate = airDate,
                Artist= Artist %>% 
                  str_trunc(MAXLEN,"right") %>% 
                  str_to_title() %>% 
                  str_extract("[\\S ]+") %>%         #get rid of extra lines in mutiple line cells
                  str_extract("[^\\(]+") %>%         #dump parentheticals
                  str_trim(),
                Title =  Title %>% 
                  str_trunc(MAXLEN,"right") %>% 
                  str_to_title() %>%
                  str_extract("[\\S ]+[^()]") %>% 
                  str_extract("[^\\(]+") %>%
                  str_trim()
      ) %>% 
      filter(Artist != '') %>% 
      filter(!is.na(Artist))
    print(playlist[1:5, ])
    
  }
  
  return(playlist)
}


#-------------- MAIN -----------------
#DJURLs<-getDJURLs()
#DJKey<-getShowNames(DJURLs)
#load(file='djkey.rdata')
music_djs<-DJKey %>% 
  select(DJ) %>% 
  anti_join(data_frame(DJ=excludeDJs)) %>% 
  as.list()
playlistURLs<-getDJPlaylistURLs(music_djs)
showCounts<-playlistURLs %>% 
  group_by(DJ) %>% 
  summarise(showCount=n()) %>% 
  arrange(desc(showCount))
DJKey<-left_join(DJKey,showCounts)
#limit analysis to DJs with at least numShows shows and take the last numshows shows.
numShows <- 50
# non-music shows
excludeDJs <-
  c('SD',
    'HA',
    'BC',
    'AF',
    'CP',
    'HP',
    'JP',
    'GM',
    'DC',
    'CC',
    'DU',
    'ES',
    'LW',
    'IM',
    'LL',
    'NW',
    'NP',
    'ZZ',
    'FC',
    'SY',
    'TI',
    'LK',
    'TP',
    'RC',
    'VC')
djList <- DJKey %>% 
  filter(showCount > numShows, !(DJ %in% excludeDJs)) %>%
  pull(DJ)
#djList<-filter(DJKey,showCount>numShows-1) %>%select(DJ) %>% .[,1]

#playlists_raw = data_frame()
for (dj in djList) {
  plURLs <- playlistURLs %>%
    filter(DJ == dj) %>%
    rowid_to_column() %>% 
    filter(rowid>51) %>% 
    select(playlistURL)
  for (n in 1:nrow(plURLs)){
    plURL<-plURLs[n,1]
    print(paste(dj, n, plURL,Sys.time()))
    if (!is.na(pull(plURLs[1,1]))){
      playlists_raw <- bind_rows(playlists_raw, testgetPlaylist(plURL, dj))
    }
  }
  #save to disk after each dj
  save(playlists_raw,file="playlists_raw.Rdata")
}

bad_Tables<-anti_join(tibble(DJ=djList),testPL)
