# scrape selected old playlists with custom routines

library(rvest)
library(stringr)
library(tidyverse)
library(tokenizers)
# ----------------------------------------------
ROOT_URL<-"https://www.wfmu.org"

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
getDJShowName_and_URLs <- function(){
  # not working yet
  rawDJURLs<- read_html(paste(ROOT_URL,"/playlists",sep=""))
  # get the urls of the each DJs RSS playlist feed
  t<-rawDJURLs%>%html_nodes(xpath='//html//body//center[2]//table[1]//table//a[contains(.,"Playlists")]') %>% 
    html_attr(name="href")
  
  show_block<-rawDJURLs %>%
    html_nodes(xpath='//html//body//center[2]//table[1]//table//span') %>% 
    html_attr(name="id") %>% str_extract("[A-Z]{2}$") 
  
  dj_code<-rawDJURLs %>%
    html_nodes(xpath='//html//body//center[2]//table[1]//table//span') %>% 
    html_attr(name="id") %>% str_extract("[A-Z]{2}$") 
  
  dj_showname <-rawDJURLs %>% 
    html_nodes(xpath='//html//body//center[2]//table[1]//table//span//following-sibling::b') %>% 
    html_text()
  
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
  DJKey <- data.frame()
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
  DJKey$onSched <- 'YES'
  DJKey$onSched[which(DJKey$DJ %in% getDJsOffSched())]<-'NO'
  #strip "WFMU" and "Playlists and Archives" and some punctuation
  DJKey$ShowName<-str_replace_all(DJKey$ShowName,"(P|p)laylists (and|&) (A|a)rchives","")
  DJKey$ShowName<-str_replace_all(DJKey$ShowName,"-","")
  DJKey$ShowName<-str_replace_all(DJKey$ShowName,"(P|p)laylist|(R|r)ecent","")
  DJKey$ShowName<-str_replace_all(DJKey$ShowName,"WFMU|wfmu","")
  DJKey$ShowName<-str_replace_all(DJKey$ShowName,"The ","")
  DJKey$ShowName<-str_trim(DJKey$ShowName)
  

  return (DJKey)  
  #save(DJKey,file="DJKey.rdata")
}

# -------------get the URLs of the playlist pages for a DJ ----------
#should work to delve into earlier years
get_playlist_page_URLs<-function(url_suffix) {
  #first call should be the base DJ page with links to any earlier year playlist lists
  if (str_length(url_suffix)==2){
    dj<-url_suffix
    latest_url<-paste0("/playlists/",url_suffix)
    url_suffix<-latest_url
  }
  singleDJ<- read_html(paste0("http://wfmu.org",url_suffix))
  #this assumes the earlier year playlist links are of the form
  # wfmu.org/playlists/<dj><year>/
  pl_url1<-singleDJ %>%
    html_nodes(xpath=paste0("//a[contains(@href,'playlists/",dj,"')]")) %>%
    html_attr("href")
  pl_url2<-singleDJ %>%
    html_nodes(xpath=paste0("//a[contains(@href,'Playlists/",dj,"')]")) %>%
    html_attr("href")
  pl_url<-c(latest_url,pl_url1,pl_url2) %>% 
    str_remove_all(pattern = ROOT_URL) %>% 
    unique()
  
return(pl_url)
}

# ------------------------------------------------------
#should work to delve into earlier years
get_playlist_page_URLs_old_Hova<-function(url_custom ="http://wfmu.org/hova/oldplists.html"){
  singleDJ<- read_html(url_custom)
  #this assumes the earlier year playlist links are of the form
  # wfmu.org/playlists/<dj><year>/
  pl_url<-singleDJ %>%
    html_nodes(xpath="//a/@href") %>% 
    html_text()
  pl_url <- tibble(DJ = "HN",playlistURL = paste0("/hova/",pl_url[grep("[0-9].html",pl_url)]))

  return(pl_url)
}
#------------------------------------------------------------------
# Old Greasy Kid stuff playlist URLs
getGKPlaylistURLs<-function(music_djs) {
  DJ_playlists = NULL
  dudList<-NULL
  xpath = "//a[contains(@href,'playlists/shows')]"
  #DJKey = data.frame()
  for (dj in music_djs) {
    print(dj)
    url_suffixes<-get_playlist_page_URLs(dj)
    for (u in url_suffixes){
      print(u)
      singleDJ<- read_html(paste0("https://wfmu.org",u))
      pl<-singleDJ %>%
        html_nodes(xpath=xpath) %>%
        html_attr("href")
      #format for newer shows
      pl<-as.character(na.omit(pl[str_detect(pl,"playlists/shows")]))
      # format for older shows
      if (length(pl)<1) {
        xpath = "//a[contains(@href,'gks')]"
        #pl<-as.character(na.omit(pl[str_detect(pl,"Playlist")]))
        pl<-singleDJ %>%
          html_nodes(xpath=xpath) %>%
          html_attr("href")
        pl <- paste0("/Playlists/GK/",pl)
        
      }
      
      # get show dates and titles
      sn <- singleDJ%>%
        html_nodes(xpath=paste0(xpath,"/child::node()")) %>% 
        html_text() %>% 
        str_remove_all("\n") %>% 
        stringi::stri_remove_empty()
      
      #assume a full URL is a fill-in DJ.  We omit these from the analysis
      pl<-pl[!str_detect(pl,"http")]
      
      playlistURL<-pl %>% as.character()
      #omit shows without valid playlists.  Talk shows?
      if (length(playlistURL)>0) {
        DJ_playlists = bind_rows(DJ_playlists, tibble(DJ=dj,show_title=sn,playlistURL = playlistURL))
      } else { 
        print("DUD")
        dudList<-c(dudList,dj) }
      
    }
  }  
  
  return(DJ_playlists)
}

# ---------------------------------------------------
# get the URLs of the playlists for a DJ
getDJPlaylistURLs<-function(music_djs) {
  DJ_playlists = NULL
  dudList<-NULL
  #DJKey = data.frame()
  for (dj in music_djs) {
    print(dj)
    url_suffixes<-get_playlist_page_URLs(dj)
    for (u in url_suffixes){
      print(u)
      singleDJ<- read_html(paste0("http://wfmu.org",u))
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
      #omit shows without valid playlists.  Talk shows?
      if (length(playlistURL)>0) {
        DJ_playlists = bind_rows(DJ_playlists, tibble(DJ=dj,playlistURL = playlistURL))
      } else { 
        print("DUD")
        dudList<-c(dudList,dj) }
      
    }
  }  
  
  return(DJ_playlists)
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
    save(allDJArtists,file="allDJArtists.rdata")
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
get_playlist_HN <-function(plURL,DJ){
  wholepage <- read_html(paste0(ROOT_URL, plURL))
  #try to pull out the show date.  assume first date in text on page is the show date
  AirDate <- wholepage %>%
    html_text() %>%
    str_extract('[A-Za-z]+ [0-9]{1,2}, [0-9]{4}') %>%
    as.Date("%B %d, %Y")
  if (is.na(airDate)) {
    #try something else
    AirDate <- wholepage %>%
      html_text() %>%
      str_extract('[0-9]{1,2} [A-Za-z]+ [0-9]{4}') %>%
      as.Date("%d %B %Y")
    
  }
  
  plraw <- NULL
  #hand-rolled
  #simplest case. A table with obvious header names
  if (!is.na(wholepage %>% html_node(xpath = "//div[@class='songlist']"))) {
    #assume first field is title and second is artist separated by colon
    #doing it it two steps assure same number of artists and titles
    title_artist<-wholepage %>% 
      html_nodes(xpath="//div[@class='songlist']") %>% 
      html_text() %>% 
      str_extract_all("\n[\\S ]+: [\\S ]+\n") %>% 
      .[[1]] 
    
    
    Artist<-title_artist %>% 
      str_extract_all("\n[\\S ]+: ") %>% 
      str_replace_all("(\n)|(: )","")
    
    Title<-title_artist %>% 
      str_extract_all(": [\\S ]+\n") %>% 
      str_replace_all("(\n)|(: )","")
    plraw<-tibble(DJ, AirDate, Artist,Title)
  }
}

#------------------------------------------------------------------
get_playlist_HN_old <-function(plURL,DJ){
  wholepage <- read_html(paste0(ROOT_URL, plURL))
  #try to pull out the show date.  assume first date in text on page is the show date
  AirDate <- wholepage %>%
    html_text() %>%
    str_extract('[A-Za-z]+ [0-9]{1,2}, [0-9]{4}') %>%
    as.Date("%B %d, %Y")
  if (is.na(airDate)) {
    #try something else
    AirDate <- wholepage %>%
      html_text() %>%
      str_extract('[0-9]{1,2} [A-Za-z]+ [0-9]{4}') %>%
      as.Date("%d %B %Y")
    
  }
  
  
  plraw <- NULL
  #hand-rolled
  #simplest case. A table with obvious header names
  if (!is.na(wholepage %>% html_node(xpath = "//div[@class='songlist']"))) {
    #assume first field is title and second is artist separated by colon
    #doing it it two steps assure same number of artists and titles
    title_artist<-wholepage %>% 
      html_nodes(xpath="//div[@class='songlist']") %>% 
      html_text() %>% 
      str_extract_all("\n[\\S ]+: [\\S ]+\n") %>% 
      .[[1]]
  } else {
    nodes <- wholepage %>% html_nodes(xpath="//font[@size='-1']/child::node()")
    artist_index <- nodes %>% html_name() %>% 
      match("b")
    artist_index <- which(artist_index==1,arr.ind = TRUE)
    title_index <- artist_index + 1
    plraw <- tibble(DJ, AirDate, Artist = nodes[artist_index] %>% html_text(trim = TRUE),
                    Title = nodes[title_index] %>% html_text(trim = TRUE))
  }
  return(plraw)
}

# ------------------------------------------------------
get_playlist_GK_old <-function(plURL,DJ){
  wholepage <- read_html(paste0(ROOT_URL, plURL))
  # tests of different playlist web page styles
  # wholepage_new <- read_html("https://wfmu.org/playlists/shows/20644") %>%
  #   xml_find_all(".//body") %>% 
  #   xml_serialize(NULL)
  # wholepage_middle <- read_html("https://www.wfmu.org/Playlists/GK/gks.001111.html")  %>%
  # xml_find_all(".//body") %>% 
  #   xml_serialize(NULL)
  # wholepage_old <- read_html("https://www.wfmu.org/Playlists/GK/gks.970823.html") %>%
  # xml_find_all(".//body") %>% 
  #   xml_serialize(NULL)
  #wholepage <- xml_unserialize(wholepage_old)
  
  # parsing artist and title is dependent on <br> tag being between songs and EITHER
  # the artist is in bold face with the <b> tag or the artist is in ALL CAPS.
  # the <b> tag is used here and the case is detected later in the routine.
  wholepage %>% xml_find_all(".//br") %>%
    xml_add_sibling("p", "\n song_delimiter \n") %>%
    {.}
  wholepage %>% xml_find_all(".//b") %>%
    xml_add_sibling("p", "\n artist_title_divide \n") %>%
    {.}
  
  
  #try to pull out the show date.  assume first date in text on page is the show date
  AirDate <- wholepage %>%
    html_text() %>%
    str_extract('[A-Za-z]+ [0-9]{1,2}, [0-9]{4}') %>%
    as.Date("%B %d, %Y")
  if (is.na(AirDate)) {
    #try something else
    AirDate <- wholepage %>%
      html_text() %>%
      str_extract('[0-9]{1,2} [A-Za-z]+ [0-9]{4}') %>%
      as.Date("%d %B %Y")
    
  }
  # print(AirDate) #DEBUG
  text <- wholepage %>% 
    html_text() %>% 
    # put newlines where they belong if needed then separate
    str_replace_all('\\"',"\n") %>%
    str_replace_all(',',"") %>%
    tokenize_regex("\n") %>%
    unlist() %>%
    enframe(name="rownum") %>%
    mutate_if(is.character, trimws) %>% 
    filter(str_length(value)>1) %>% 
    {.}
  
  first_song_line <- text %>%
    filter(value == "Greasy Kid Stuff") %>% 
    pull(rownum) %>% -1
  if (length(text$value[first_song_line]) < 2) first_song_line <- first_song_line - 1
  
  # I include fill-in shows that don't start with the song "greasy kid stuff"
  if (is_empty(first_song_line)){
    n <- 1
    repeat({
      print(text[n:(n+9),])
      first_song_line <- readline("Can't find first song.  Enter a line number to start on. <enter> for more")
      if (first_song_line == ""){
        n <- n + 10
      } else{ 
        first_song_line <- as.numeric(first_song_line)
        break
      }
    })
  }
  
  last_song_line <- text %>%
    filter(str_detect(value,"Previous playlist"))
  if (nrow(last_song_line) == 0){
    last_song_line <- text %>%
      filter(str_detect(str_to_lower(value),"gks main"))
  }    
  
  last_song_line <- last_song_line %>% pull(rownum)
  
  artist_title <- text  %>% 
    filter(rownum >= first_song_line) %>% 
    filter(rownum < last_song_line) %>% 
    select(-rownum) %>% 
    # relabel row column for debugging but not needed otherwise
    rowid_to_column(var="rownum")
  
  
  # this is way too complicated but the variances across all the playlist pages is too great.
  artist <- list()
  title <- list()
  n <- 1
  row_state <- "artist"
  new_title <- ""
  new_artist <- ""
  # use "while" instead of "for" because the count of rows is dynamic
  while (n < nrow(artist_title)){
    cur_row <- artist_title$value[n]
    #detect artist name by ALL CAPS
    if (cur_row == str_to_upper(cur_row)){
      #insert a row after the artist name with label
      artist_title <- rbind(artist_title[1:n,],
                            data.frame(rownum=0,value="artist_title_divide"),
                            artist_title[-(1:n),])
      
    } 
    if (cur_row == "song_delimiter"){
      # on to next song. Close out build of title
      # print(paste(new_artist,new_title)) # DEBUG
      title <- c(title,str_trim(new_title))
      artist = c(artist,str_trim(new_artist))
      row_state <- "artist"
      new_artist <- ""
      new_title <- ""
    }
    if (cur_row  == "artist_title_divide"){
      row_state = "title"
    }
    if ((cur_row != "song_delimiter") & 
        (cur_row != "artist_title_divide") &
        (row_state == "artist")){
      new_artist <- paste(new_artist,cur_row)
    }
    if ((cur_row != "song_delimiter") & 
        (cur_row != "artist_title_divide") &
        (row_state == "title")){
      new_title <- paste(new_title,cur_row)
    }
    n <- n+ 1
  }
  
  playlist <- tibble(DJ="GK",AirDate=AirDate,Artist=unlist(artist),Title=unlist(title)) %>% 
    mutate_at(vars(c("Artist","Title")),str_to_title)
  return(playlist)
}

#------------------------------------------------------------------

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
get_playlist <- function(plURL, dj) {
  
  wholepage <- read_html(paste0(ROOT_URL, plURL))
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
#  if (airDate < most_recent_date-7) {
#    print("Back Far Enough")
#    return(NULL)
#  }
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
          print("DUD. Cand't find header")
          plraw <- NULL
        }
      }
    }
  }
  
  if (is.null(plraw)) {
    # no song class, now what? is it a table? try to  find header
    #seems like cellspacing means its a row column thing
    pl_table<-wholepage %>% html_node(xpath = "//table[@cellspacing and @cellpadding]")
    num_rows<-pl_table %>% html_nodes("tr") %>% length()
    if (num_rows>2) {
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
      }
    }
  }
      
# SPECIAL DJ TREATMENT
  if (is.null(plraw)){
    #try idiosyncratic djs
    if (dj=="TW") plraw<-try_BK(wholepage)
    if (dj=="HN") plraw<-try_HN(wholepage)
    if (is.null(plraw)) print("DUD. Can't find header")
    #final dead end
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
# save(DJKey,file="DJKey.rdata")
#load(file='djkey.rdata')


playlists_DJ <- NULL
dj <- "GK"
# playlistURLs<-getGKPlaylistURLs(dj)


#playlistURLs <- get_playlist_page_URLs_old_Hova()
#showCounts<-playlistURLs %>% 
#  group_by(DJ) %>% 
#  summarise(showCount=n()) %>% 
#  arrange(desc(showCount))
#DJKey<-left_join(DJKey,showCounts)
#save(DJKey,file="DJKey.rdata")

#limit analysis to DJs with at least numShows shows.
# This also excludes DJs where we couldn't extract valid playlist URLs.
#numShows <- 10
# non-music shows
#djList <- DJKey %>% 
#  filter(showCount > numShows, !(DJ %in% excludeDJs)) %>%
#  pull(DJ)

  plURLs <- playlistURLs$playlistURL
  for (plURL in plURLs){
    print(paste(dj, plURL,Sys.time()))
      playlist<-get_playlist_GK_old(plURL, dj)
      playlists_DJ <- bind_rows(playlists_DJ, playlist)
    }
  # save to disk after each dj
  # playlists_raw <- bind_rows(playlists_raw,playlists_DJ)  
  # save(playlists_raw,file="playlists_raw.rdata")

fix_rows_gk <- function(playlist){
  # fix dashes
  playlist <- playlist %>% 
    separate(Artist, into = c("Artist2","Title2"),remove = FALSE,sep=" - ") %>% 
    mutate(Title = ifelse(Title == "" & !is.na(Title2),Title2,Title)) %>% 
    mutate(Title = ifelse(Title=="",Artist2,Title)) %>% 
    mutate(Artist = ifelse((Artist =="" & Title == "Greasy Kid Stuff"),"The Jack Mormons",Artist)) %>%
    mutate(Artist = str_remove(Artist,"^[0-9]{1,2}\\. ")) %>% 
    select(-Artist2,-Title2) %>% 
    filter(Title != "") %>% 
    mutate(Artist = ifelse(str_detect(Artist,"nknown"),"Unknown",Artist)) %>%
    mutate(Artist = ifelse(Artist == "","Unknown",Artist)) %>%
    mutate(Artist = ifelse(str_detect(Title,"& Kate"),"Max & Kate's Review",Artist)) %>% 
    mutate(Title = ifelse(str_detect(Title,"& Kate"),str_remove(Title,"^.+: "),Title)) %>% 
    {.}
  return(playlist)
}

#bad_Tables<-anti_join(tibble(DJ=djList),playlists_raw)


# Add first show and last show to DJKey
#FirstShow<-playlists %>% 
#  group_by(DJ) %>% 
#  select(DJ,AirDate) %>% 
#  distinct() %>% 
#  top_n(-1) %>% rename(FirstShow=AirDate)

#LastShow<-playlists %>% 
#  group_by(DJ) %>% 
#  select(DJ,AirDate) %>% 
#  distinct() %>% 
#  top_n(1) %>% rename(LastShow=AirDate)

#DJKey <- DJKey %>% 
#  left_join(FirstShow,by="DJ") %>% 
#  left_join(LastShow,by="DJ")

#cleanup
#rm(LastShow,FirstShow)


