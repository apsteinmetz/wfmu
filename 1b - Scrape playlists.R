# scrape playlists

library(rvest)
library(stringr)
library(xml2)
library(tidyverse)
library(progress)
library(duckplyr)

# ----------------------------------------------
ROOT_URL<-"http://wfmu.org"

#-------------------------------------------------
# # Get all Artists ever played by a DJ
# #WFMU maintains this as a separate page
# getDJArtistNames<-function(DJURLs) {
#   # scrape artist names for all DJs from the link at the bottom of each DJ page
#   allDJArtists<-data.frame()
#   URL_BRANCH<- "/artistkeywords.php/"
#   for (page in DJURLs) {
#     singleDJ<- read_html(page)
#     showName <- html_node(singleDJ,"title")%>%html_text()
#     showName <- gsub("\n","",sub("Playlists and Archives for ","",showName))
#     DJ <- sub("http://wfmu.org/playlists/","",page)
#     djKey<-rbind(djKey,data.frame(DJ=DJ,ShowName=showName))
#     print(showName)
#     artistListPage <- paste(ROOT_URL,URL_BRANCH,DJ, sep="")
#     artistList<-read_html(artistListPage)%>%html_node(xpath="//body/div")%>%html_text()%>%str_split("\n")
#     DJArtists<-data.frame(DJ,artistRaw=unlist(artistList))
#     if (nrow(DJArtists) >0) allDJArtists = rbind(allDJArtists,DJArtists)
#     #remove factor level of DJs with no artists
#     save(allDJArtists,file = "data/allDJArtists.rdata")
#   }
#   return(allDJArtists)
# }  

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
get_playlist <- function(plURL="/playlists/shows/93065", dj = "WA") {
  
  wholepage <- tryCatch(
    read_html(paste0(ROOT_URL, plURL)),
    error = function(e){NA}) #handle 404 errors
  if(is.na(wholepage)){
    return(bind_rows(playlists_raw,tibble(DJ="",AirDate=as.Date(NA),Artist="",Title="")))
  }
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
  if (airDate < most_recent_date-7) {
    print("Back Far Enough")
    return(NULL)
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
    }
    
# new 2020 style headers.  Nobody told me about it!
  if (is.null(plraw)){
    artists <- wholepage %>% html_nodes(xpath = "//td[@class='song col_artist']") %>% html_text() %>% 
      str_remove_all("\\n")
    titles <- wholepage %>% html_nodes(xpath = "//td[@class='song col_song_title']") %>% html_text() %>% 
      str_remove_all("\\n")
    plraw <- tibble(Artist = artists,Title = titles)
    if (nrow(plraw) == 0) plraw <- NULL
  }  
  
  plraw<-fixHeaders(plraw)
  # final clean up if we have something
  if (is.null(plraw)) {
    playlist <- NULL
    print("DUD")
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
                  str_trim() |> 
                  iconv(to = "UTF-8", sub = "byte"),
                Title =  Title %>% 
                  str_trunc(MAXLEN,"right") %>% 
                  str_to_title() %>%
                  str_extract("[\\S ]+[^()]") %>% 
                  str_extract("[^\\(]+") %>%
                  str_trim() |> 
                  iconv(to = "UTF-8", sub = "byte")
      ) %>% 
      filter(Artist != '') %>% 
      filter(!is.na(Artist))
    # just to track progress
    if (is.null(playlist)){
      print("No Playlist")
    } else {
      print(playlist[1:5, ]) 
    }
  }
  return(playlist)
}
#-------------- MAIN -----------------
djKey<- df_from_parquet("data/djKey_prelim.parquet")
playlistURLs<-df_from_parquet("data/playlistURLs.parquet")
playlists_raw <- df_from_parquet("data/playlists_raw.parquet") |> 
  as_tibble()

djList <- djKey %>% 
  pull(DJ)

#careful not to trash intermediate results!
# load("data/playlists_raw.rdata")
UPDATE_ONLY =TRUE
if (UPDATE_ONLY) {
  #assume at most 5 shows per week
  #most shows are 1/week except the morning show
  most_recent_dates<-playlists_raw %>% 
    arrange(AirDate) %>% 
    group_by(DJ) %>% 
    summarise(most_recent = max(AirDate))
  most_recent_date<-max(playlists_raw$AirDate,na.rm = TRUE)
  #go_back_num<- as.integer(round ((Sys.Date() - most_recent_date) * 5/7))
  #print(paste("Updating ONLY last",go_back_num,"Shows"))
} else{
  go_back_num<- Sys.Date() - as.Date("1900-01-01") #arbitrarily far back so it's not a binding constraint
}
 
djList_temp<-djList
#example way to restart if failure occurs in middle of list at,say dj "VR"
#djList_temp<-djList[match("RQ",djList):length(djList)]
for (dj in djList_temp) {
  plURLs <- playlistURLs %>%
    filter(DJ == dj) %>%
    rowid_to_column() %>% 
    select(playlistURL)
  if (UPDATE_ONLY) {
    go_back_num<- round(as.integer(Sys.Date() - filter(most_recent_dates,DJ==dj)$most_recent) * 5/7)
    if (length(go_back_num) == 0) {
      #this means dJ is not in playlist_raw so initiate an new DJ
      go_back_num <- 100
    }
    print(paste("Updating ONLY last",go_back_num,"Shows"))
  } else{
    go_back_num<-nrow(plURLs)
  }
  for (n in 1:go_back_num){
    plURL<-plURLs[min(n,nrow(plURLs)),1]
    print(paste(dj, n, plURL,Sys.time()))
    if (!is.na(pull(plURLs[1,1]))){
      playlist<-get_playlist(plURL, dj)
      if(!is.null(playlist)){
        playlists_raw <- bind_rows(playlists_raw, playlist)
      }
    }
    if (is.null(playlist)) break #done with this DJ
  }
  #save to disk after each dj
  df_to_parquet(playlists_raw, "data/playlists_raw.parquet")
}

bad_Tables<-anti_join(tibble(DJ=djList),playlists_raw) %>% left_join(djKey)

playlists_raw<-playlists_raw %>% 
  filter(Artist != Title) %>% #single column span across table.  Not a song.
  distinct()

df_to_parquet(playlists_raw, "data/playlists_raw.parquet")
right_join(djKey,bad_Tables) %>% save(file = "data/bad_tables.rdata")

