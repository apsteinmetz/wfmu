# temp
library(tidyverse)
library(tidytext)
library(tokenizers)
library(rvest)
library(xml2)
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

