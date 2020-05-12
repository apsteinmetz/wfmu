# temp
library(tidyverse)
library(tidytext)
library(tokenizers)
library(rvest)
library(xml2)

#existingDF <- rbind(existingDF[1:r,],newrow,existingDF[-(1:r),])

# wholepage_new <- read_html("https://wfmu.org/playlists/shows/20644") %>%
#   xml_find_all(".//body") %>% 
#   xml_serialize(NULL)
# wholepage_middle <- read_html("https://www.wfmu.org/Playlists/GK/gks.001111.html")  %>%
# xml_find_all(".//body") %>% 
#   xml_serialize(NULL)
# wholepage_old <- read_html("https://www.wfmu.org/Playlists/GK/gks.970823.html") %>%
# xml_find_all(".//body") %>% 
#   xml_serialize(NULL)

# parsing artist and title is dependent on <br> tag being between songs and EITHER
# the artist is in bold face with the <b> tag or the artist is in ALL CAPS.
# the <b> tag is used here and the case is detected later in the routine.
wholepage <- xml_unserialize(wholepage_old)
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
print(AirDate)
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
  print(text)
  first_song_line <- as.integer(readline("Can't find first song.  Enter a line number to start on. "))
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
    print(paste(new_artist,new_title))
    title <- c(title,new_title)
    artist = c(artist,new_artist)
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
    new_artist <- paste0(new_artist,cur_row)
  }
  if ((cur_row != "song_delimiter") & 
      (cur_row != "artist_title_divide") &
      (row_state == "title")){
    new_title <- paste0(new_title,cur_row)
  }
  n <- n+ 1
}

playlist <- tibble(DJ="GK",AirDate=AirDate,Artist=unlist(artist),Title=unlist(title)) %>% 
   mutate_at(vars(c("Artist","Title")),str_to_title)
