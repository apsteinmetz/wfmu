# scrape playlists

library(rvest)
library(stringr)
library(xml2)
library(tidyverse)
library(progress)
library(duckplyr)




# ----------------------------------------------
ROOT_URL<-"http://wfmu.org"
testurl <- "http://wfmu.org/playlists/KF"
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

# get_other_shownames for the given DJ
get_other_shownames <- function(url,base_showname){
  # read_html with error checking
  html <- try(xml2::read_html(url), silent = TRUE)
  if (inherits(html, "try-error")){
    html <- NULL
  } 
  
  if (is.null(html)) {
    return(NULL)
  }
  all_shownames <- html %>%
    html_nodes(".KDBprogram + a") |>
    html_text()
  # remove the current show name from the list
  shownames <- all_shownames[all_shownames != base_showname] |> 
  paste0(collapse = '\n')
  return(shownames)
}
#---------------------------------------------------
# get the shownames for a DJ
getShowNames<-function(DJURLs) {
  pb <- progress_bar$new(
    format = "  Getting Show :what [:bar] :percent eta: :eta",
    clear = FALSE, total = length(DJURLs))
  djKey_raw <- data.frame()
  for (page in DJURLs) {
    singleDJ<- read_html(page)
    showName <- html_node(singleDJ,"title") %>% html_text()
    showName <- gsub("\n","",sub("Playlists and Archives for ","",showName))
    showName<-str_replace(showName,'WFMU:',"")
    showName<-str_replace_all(showName,':Playlists and Archives',"")
    DJ <- sub("http://wfmu.org/playlists/","",page)
    pb$tick(tokens = list(what = DJ))
    profileURL<-singleDJ%>%
      html_nodes(xpath="//a[contains(@href,'profile')]") %>%
      html_attr("href") |> pluck(1)
    # if profile URL is not found, use the DJ URL
    if (length(profileURL)==0){
      profileURL<-page
      other_shownames <- "none"
    } else {
      other_shownames <- get_other_shownames(profileURL,showName)
      if (length(other_shownames)==0) other_shownames <- "none"
    }
    # print(DJ)
    djKey_raw<-rbind(djKey_raw,data.frame(DJ=DJ,
                                  ShowName=showName,
                                  profileURL=profileURL,
                                  other_shows = other_shownames))
    
  }
  # now identifty those DJs which are currently ON MIC
  djKey_raw$onSched <- 'YES'
  djKey_raw$onSched[which(djKey_raw$DJ %in% getDJsOffSched())]<-'NO'
  #strip "WFMU" and "Playlists and Archives" and some punctuation
  djKey_raw$ShowName<-str_replace_all(djKey_raw$ShowName,"(P|p)laylists (and|&) (A|a)rchives","")
  djKey_raw$ShowName<-str_replace_all(djKey_raw$ShowName,"-","")
  djKey_raw$ShowName<-str_replace_all(djKey_raw$ShowName,"(P|p)laylist|(R|r)ecent","")
  djKey_raw$ShowName<-str_replace_all(djKey_raw$ShowName,"WFMU|wfmu","")
  djKey_raw$ShowName<-str_replace_all(djKey_raw$ShowName,"The ","")
  djKey_raw$ShowName<-str_trim(djKey_raw$ShowName)
  

  return (djKey_raw)  

  # extraction method
  # djKey_raw$other_shows[[1]] |> paste0(collapse = '\n') |> cat()
  
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
  pl_url<-singleDJ %>%
    html_nodes(xpath=paste0("//a[contains(@href,'playlists/",dj,"')]")) %>%
    html_attr("href")
  # combine root with children but remove dupes and redundant URLs
   pl_url<-c(latest_url,pl_url) %>% 
     unique() %>% 
     str_remove_all("http.+") %>%
     stringi::stri_remove_empty()
return(pl_url)
}


# get_playlist_page_suffixes <- function(music_djs){
#   url_suffixes <- NULL
#   for (dj in music_djs) {
#     url_suffixes <- c(url_suffixes,get_playlist_page_URLs(dj))
#   }
#   return(url_suffixes)
# }

#---------------------------------------------------
# get the URLs of the playlists for a DJ
getDJPlaylistURLs <- function(music_djs) {
  pb1 <- progress_bar$new(format = "  Getting Playlist URL for :what [:bar] :percent eta: :eta",
                         clear = FALSE,
                         total = length(music_djs))
  
  DJ_playlists = NULL
  dudList <- NULL
  #djKey = data.frame()
  for (dj in music_djs) {
    pb1$tick(tokens = list(what = dj))
    url_suffixes <- get_playlist_page_URLs(dj)
    for (u in url_suffixes) {
      # pb$tick(tokens = list(what = str_remove(u, "/playlists/")))
      singleDJ <- read_html(paste0("http://wfmu.org", u))
      pl <- singleDJ %>%
        html_nodes(xpath = "//a[contains(@href,'playlists/shows')]") %>%
        html_attr("href")
      #format for newer shows
      pl <- as.character(na.omit(pl[str_detect(pl, "playlists/shows")]))
      # format for older shows
      if (length(pl) < 1)
        pl <- as.character(na.omit(pl[str_detect(pl, "Playlist")]))
      
      #assume a full URL is a fill-in DJ.  We omit these from the analysis
      pl <- pl[!str_detect(pl, "http")]
      
      playlistURL <- pl %>% as.character()
      #omit shows without valid playlists.  Talk shows?
      if (length(playlistURL) > 0) {
        DJ_playlists = bind_rows(DJ_playlists, tibble(DJ = dj, playlistURL = playlistURL))
        dudflag <- "OK "
      } else {
        dudflag <- "DUD"
        dudList <- c(dudList, dj)
      }
    }
  }
  return(DJ_playlists)
}

# get profile page URL by extracting href containing the word "profile" from the DJ page
getDJProfileURLs<-function(DJURLs) {
  pb <- progress_bar$new(total = length(DJURLs))
  DJProfileURLs = NULL
  for (page in DJURLs) {
    singleDJ<- read_html(page)
    DJ <- sub("http://wfmu.org/playlists/","",page)
    profileURL<-singleDJ%>%
      html_nodes(xpath="//a[contains(@href,'profile')]") %>%
      html_attr("href")
#    profileURL<-as.character(na.omit(profileURL[str_detect(profileURL,"profile")]))
    if (length(profileURL)>0) {
      DJProfileURLs = bind_rows(DJProfileURLs, tibble(DJ=DJ,profileURL = profileURL))
    }
    pb$tick()
  }
  return(DJProfileURLs)
}

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

#-------------- MAIN -----------------
# spoken word shows
excludeDJs <-
  sort(c('SD',
         'AF',
         'HA',
         'BC',
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
         'GJ',
         'NP',
         'ZZ',
         'FC',
         'SY',
         'TI',
         'LK',
         'TP',
         'RC',
         'TD',
         'B3',
         'VC'))
DJURLs<-getDJURLs()
# remove djurls in excludeDJs
DJURLs <-DJURLs[!str_detect(DJURLs,paste0(excludeDJs,collapse="|"))]
djKey<-getShowNames(DJURLs)


playlistURLs<-getDJPlaylistURLs(djKey$DJ)
showCounts<-playlistURLs %>%
  group_by(DJ) %>%
  summarise(showCount=n()) %>%
  arrange(desc(showCount))
djKey<-left_join(djKey,showCounts) %>% 
  drop_na() |> 
  as_tibble()

#limit analysis to DJs with at least numShows shows.
# This also excludes DJs where we couldn't extract valid playlist URLs.
numShows <- 10
# non-music shows
djKey <- djKey %>%
  filter(showCount > numShows, !(DJ %in% excludeDJs))


df_to_parquet(djKey,"data/djKey_prelim.parquet")
# save playlistURLs as parquet
df_to_parquet(playlistURLs,"data/playlistURLs.parquet")

