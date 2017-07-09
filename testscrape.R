library(rvest)
library(stringr)
library(xml2)
library(tidyverse)



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
  n<-names(pl) %in% altArtistNames
  t<-names(pl) %in% altTitleNames
  if (any(which(n))){
    names(pl)[n] <- "Artist"
  } else{
    pl$Artist=NA
  } 
if (any(which(t))){
  names(pl)[t] <- "Title"
} else{
  pl$Title=NA
}
  
  return(pl)
}

#--------------------------------------------------------------------
testgetPlaylist <- function(plURLs, dj) {
  i <- 1
  print(paste(dj, i, plURLs[i, 1]))
  
  wholepage <- read_html(paste(ROOT_URL, plURLs[i, 1], sep = ''))
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
  #simplest case. A table with obvious header names
  if (!is.na(wholepage %>% html_node(xpath = "//th[@class='song']"))) {
      table_shell<-xml_new_root("table")
      plraw<-wholepage %>% 
        html_nodes(xpath="//tr[td[@class ='song']] | //tr[th[@class ='song']]")
      for (node in plraw) xml_add_child(table_shell,node)
      plraw<-table_shell %>% 
        html_node(xpath="//table") %>% 
        html_table(fill=TRUE) %>% na.omit()
    
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
    if (!is.na(pl_table)) {
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
  # final clean up if we have something
  if (is.null(plraw)) {
    playlist <- NULL
  } else {
    if (TRUE %in% is.na(names(plraw)))
      plraw <- plraw[, -which(is.na(names(plraw)))] #sometimes an NA column
    
    playlist <- plraw %>%
      fixHeaders() %>%
      select(Artist,Title) %>% 
      na.omit() %>% 
      transmute(DJ = dj, 
                AirDate = airDate,
                Artist= Artist %>% str_trunc(MAXLEN,"right") %>% str_to_title(),
                Title =  Title %>% str_trunc(MAXLEN,"right") %>% str_to_title()) %>%
      filter(Artist != '')
    print(playlist[1:5, ])
    
  }
  
  return(playlist)
}


#-------------- MAIN -----------------
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
    'IM')
djList <- filter(DJKey, showCount > numShows - 1, !(DJ %in% excludeDJs)) %>%
  select(DJ) %>% .[, 1]
#djList<-filter(DJKey,showCount>numShows-1) %>%select(DJ) %>% .[,1]

testPL = data_frame()
for (dj in djList) {
  plURLs <- playlistURLs %>%
    filter(DJ == dj) %>%
    .[1:numShows, ] %>%
    select(playlistURL)
  testPL <- bind_rows(testPL, testgetPlaylist(plURLs, dj))
  bad_Tables<-anti_join(tibble(DJ=djList),testPL)
}
