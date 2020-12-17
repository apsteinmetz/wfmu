# import image as text
library(tidyverse)
library(tesseract)
library(tokenizers)
library(lubridate)


parse_scenes_from_png <- function(file){
  
  text_raw <- tesseract::ocr(file)
  text <- text_raw %>% 
    tokenize_regex(pattern = "\\n") %>% 
    unlist()
  
  text2 <- list()
  n2 <- 1
  for (n in 1:length(text)){
    # detect start of entry
    if (str_detect(text[n],"^[0-9]{1,3}\\.")){
      text2[n2] <- text[n]
      n2 <- n2 + 1
    } else{
      # add wrapped text to previous line
      #    print(paste(text2[n2-1],text[n]))
      text2[n2-1] <- paste(text2[n2-1],text[n])
    }
  }
  
  text3 <- text2 %>% 
    unlist() %>% 
    enframe(name="chron_position") %>%
    # some temp changes to make my regexes works
    mutate(value = str_replace(value,"\\bA ","A_")) %>% 
    mutate(value = str_replace(value,"\\bAbit","A_bit")) %>% 
    # now do real work
    mutate(chron_position = as.numeric(str_extract(value, "^[0-9]{1,3}"))) %>%
    mutate(value = str_remove(value, "^[0-9]{1,3}\\. ")) %>%
    mutate(movie_name = str_extract(value,"\\b[A-Z][A-Z \\.\\-\\:]+[0-9]*")) %>% 
    mutate(movie_name = str_remove(movie_name,"[\\(][[:alnum:]]*")) %>% 
    mutate(movie_name = as.factor(str_trim(movie_name))) %>% 
    mutate(location = str_remove(value,"\\b[A-Z][A-Z \\-\\:]+[0-9]*")) %>% 
    mutate(location = str_remove(location,"\\([[:alnum:]: \\-,\\)]*$")) %>% 
    mutate(location = str_remove(location,"(of|of \\. 2|in|at|from)[ ]*$")) %>% 
    mutate(location = str_replace(location,"^A_","A ")) %>% 
    mutate(time_window = str_extract(value,"\\([[:alnum:] ;:,-]+\\)")) %>% 
    mutate(time_window = str_remove_all(time_window,"[\\(\\)]")) %>% 
    mutate(time_window = str_replace_all(time_window,"end at","stop at")) %>% 
    separate(time_window,into = c("time_start","time_stop"),sep=",",remove = FALSE) %>% 
    mutate(time_stop = ifelse(str_starts(time_start,"stop"),time_start,time_stop)) %>% 
    mutate(time_start = ifelse(str_starts(time_start,"stop"),NA,time_start)) %>% 
    mutate(time_start = ifelse(is.na(time_start)&!is.na(time_stop),"0:00:00",time_start)) %>%
    mutate_at(vars(starts_with("time")),str_remove,"(start at |stop at)") %>%
    mutate_if(is.character,str_trim) %>%
    mutate_if(is.character,str_trim) %>%
    mutate(duration_mins = round(difftime(parse_date_time(time_stop,orders=c("HMS","MS")),
                                          parse_date_time(time_start,orders=c("HMS","MS")),
                                          units="mins"),1)
    )
  return(text3)
}


scenes1 <- parse_scenes_from_png("data/mcu.png")
scenes2 <- parse_scenes_from_png("data/mcu2.png")
#So, REVISION! Replace #20-44 with all THIS, and move everything below it down six spaces. 
scenes3 <- parse_scenes_from_png("data/mcu3.png")

scenes <- scenes1[c(1:19,45:100),] %>% 
  bind_rows(scenes2) %>% 
  mutate(chron_position = ifelse(chron_position>44,chron_position+6,chron_position)) %>% 
  bind_rows(scenes3) %>% 
  arrange(chron_position) %>%
  mutate(movie_name = as.factor(movie_name))

# ----------------------------------------
# get release dates from wikipedia
if (!file.exists("data/MCU_release_dates.rdata")){
  library(rvest)
  movie_table <- read_html("https://en.wikipedia.org/wiki/List_of_Marvel_Cinematic_Universe_films") %>% 
    html_nodes("table") %>% .[2] %>%   
    html_table(fill=TRUE) %>% 
    .[[1]] %>% 
    . [,1:2] %>%
    filter(!str_detect(Film,"Phase")) %>% 
    mutate(movie_name = toupper(Film)) %>% 
    mutate(movie_name = str_remove(movie_name,"MARVEL'S ")) %>%
    mutate(movie_name =  as.factor(movie_name)) %>% 
    mutate(US_release_date = parse_date(`U.S. release date`,format = "%B %d, %Y")) %>% 
    select(movie_name,US_release_date) %>% 
    arrange(US_release_date) %>% 
    rowid_to_column(var = "release_order") %>% 
    as_tibble() %>% 
    {.}
  save(movie_table,file="data/MCU_release_dates.rdata")
} else load("data/MCU_release_dates.rdata")
# ---------------------

scenes_final <- scenes %>% 
  select(chron_position,movie_name,location,time_start,time_stop,duration_mins) %>% 
  left_join(movie_table) %>%
  arrange(chron_position) %>% 
  mutate(movie_name = as_factor(as.character(movie_name)))

  
write.csv(scenes_final,file="data/MCU_scene_chron.csv")


plot_scene_chron <- function(ordering = c("release","earliest","latest")){
  if (ordering == "release"){
    ylab = "Movie Release Order"
    scenes_final <- scenes_final %>% 
      arrange(release_order) %>% 
      mutate(movie_name = as_factor(as.character(movie_name)))
  }  
  if (ordering == "latest"){
    ylab = "Latest Scene Order"
    scenes_final <- scenes_final %>% 
      arrange(desc(chron_position)) %>% 
      mutate(movie_name = as_factor(as.character(movie_name)))
  }
  if (ordering == "earliest"){
    ylab = "Earliest Scene Order"
    scenes_final <- scenes_final %>% 
      arrange(chron_position) %>% 
      mutate(movie_name = as_factor(as.character(movie_name)))
  }
  ggplot(scenes_final,aes(chron_position,movie_name,color=movie_name)) + geom_point(size=2) + 
    theme(legend.position = "none") + 
    labs(title = "Marvel Cinematic Universe Scene Chronology",
         x = "Scene Chronology",y=ylab) + 
    geom_line()
}


ggplot(text_final,aes(chron_position,movie_name,color=movie_name)) + geom_point(size=2) + 
  theme(legend.position = "none") + 
  labs(title = "Marvel Cinematic Universe Scene Chronology",
       x = "Scene Chronology",y="Earliest Scene Order") + 
  geom_line()


ggplot(text_final,aes(chron_position,movie_name,color=movie_name)) + geom_point(size=2) + 
  theme(legend.position = "none") + 
  labs(title = "Marvel Cinematic Universe Scene Chronology",
       x = "Scene Chronology",y="Latest Scene Order") + 
  geom_line()

