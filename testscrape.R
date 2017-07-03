altArtistNames <- c('THE STOOGE', 'Band', 'Singer', 'Artist')
altTitleNames <- c('THE SONG', 'Track', 'Song')
altHeaderNames <- c(altArtistNames, altTitleNames)
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

fixHeaders <- function(pl) {
  #takes a data frame
  names(pl)[names(pl) %in% altTitleNames] <- "Title"
  names(pl)[names(pl) %in% altArtistNames] <- "Artist"
  return(pl)
}

handleBadTable <- function(wholepage) {
  headers <- wholepage %>%
    html_node(xpath = "//th[@class='song']/parent::tr") %>%
    html_text() %>%
    str_split('\n') %>%
    unlist()
  plraw <- wholepage %>%
    html_nodes(xpath = "//td[@class='song']/parent::tr") %>%
    html_text() %>%
    str_split('\n') %>%
    as.data.frame() %>%
    t() %>%
    as_data_frame()
  plraw <- plraw[, !sapply(plraw, function(x)
    (all(x == ""))), drop = F]
  # still has columns with only on-printable characters
  
  #NOT WORKING
  
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
    plraw <- wholepage %>%
      html_node(xpath = "//td[@class='song']/ancestor::table") %>%
      html_table(fill = TRUE)
  } else {
    # no 'th' but are there rows in a table with td of class=song?  get the table
    if (!is.na(wholepage %>% html_node(xpath = "//tr[@class='song']"))) {
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
      if (TRUE %in% (names(pl_table) %in% altHeaderNames)) {
        pl_raw<-pl_table
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
          print("DUD. Can't find header")
          plraw <- NULL #final dead end
        }
      }
    }
    
    # plraw_head <- wholepage %>%
    #   html_nodes(xpath = "//td[(@class='song') and not(@colspan | @align)]") %>%
    #   html_text() %>%
    #   str_replace('\n', '') %>%
    #   str_trim()
    # 
    # plraw_row <- wholepage %>%
    #   html_nodes(xpath = "//td[(@class='song') and not(@colspan | @align)]") %>%
    #   html_text() %>%
    #   str_replace('\n', '') %>%
    #   str_trim() %>%
    #   matrix(ncol = length(plraw_head), byrow = T) %>%
    #   as_data_frame()
    # 
  }
  # final clean up if we have something
  if (is.null(plraw)) {
    playlist <- NULL
  } else {
    if (TRUE %in% is.na(names(plraw)))
      plraw <- plraw[, -which(is.na(names(plraw)))] #sometimes an NA column
    
    playlist <- plraw %>%
      as_data_frame() %>%
      fixHeaders() %>%
      mutate(DJ = dj, AirDate = airDate) %>%
      select(DJ, AirDate, Artist, Title) %>%
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
    'ES')
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
}
