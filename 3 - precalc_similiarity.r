#update doc term matrix and dj similarity table for saving to server

library(tm)
library(tidyverse)
library(lubridate)
library(tidytext)
library(vegan) #similarity measures
library(duckplyr)
# load("data/playlists.rdata")
# load playlists with duckplyr
playlists <- df_from_parquet("data/playlists.parquet")

#Analyze similarity 
#-------------------------------------------------------------  
#combineAllArtists
concat_artists<- tibble()
#make sure there aren't extra levels
playlists$DJ<-factor(playlists$DJ,as.character(unique(playlists$DJ)))
for (dj in levels(playlists$DJ)){
  cat(dj," ")
  #put all words in string for each DJ
  concat_artists<-bind_rows(concat_artists,
                            tibble(
                              DJ=dj,
                              Artists= playlists%>%
                                filter(DJ==dj)%>%
                                pull(ArtistToken)%>% 
                                paste(collapse=" ")%>%
                                str_replace_all("[^A-Za-z ]","")%>%as.character()
                            ))
}
#artists should not have factor levels
#concat_artists$Artists<-as.character(concat_artists$Artists)
cat("\nBuilding DJ Corpus\n")
concat_artists<-filter(concat_artists,Artists!="") %>% distinct()
djCorpus <- VCorpus(VectorSource(concat_artists$Artists))
for (i in 1:length(djCorpus)) {
  meta(djCorpus[[i]], tag="id") <- concat_artists$DJ[i]  
  meta(djCorpus[[i]], tag="DJ") <- concat_artists$DJ[i]
}
#OR if you have the memory to pre-create
#get roughly top 400 artists when removeSparseTerms(0.80) used. top 8000 when 0.95 sparse is used
SPARSE<- 0.95 #sparsity of term document matrices

# now create Document Term Matrix where DJs are the column
djdtm<-DocumentTermMatrix(djCorpus) %>% 
  removeSparseTerms(SPARSE)
cat("Saving djdtm as rdata\n")
save(djdtm,file="data/djdtm.rdata")
#-----------------------------------------------------------

dj_similarity<-djdtm %>%
  as.matrix() %>% 
  vegdist(method="bray") %>% 
  as.matrix()
#-----------------------------------------------------------

dj_similarity_tidy <- dj_similarity %>%
  as.data.frame() %>%
  mutate(DJ1 = rownames(.)) %>%
  as_tibble() %>%
  gather(DJ2, Similarity, -DJ1) %>%
  mutate(Similarity = 1 - Similarity) %>%
  filter(DJ1 != DJ2) %>%  #remove diagonals
  group_by(DJ1) %>%
  arrange(desc(Similarity)) |>
  ungroup()
# save(dj_similarity_tidy,file='data/djsimilarity.rdata')
# save as parquet
cat("Saving djsimilarity as parquet\n")
df_to_parquet(dj_similarity_tidy, "data/djsimilarity.parquet")

# what artists make a dj different from another
cat("Computing distinctive artists\n")
dj_tf_idf <- playlists |> select(DJ, ArtistToken) |> 
    filter(ArtistToken != '') |>
  # remove ArtistToken where only numerals, probably bogus
  filter(!str_detect(ArtistToken, '^[0-9]+$')) |> 
  summarise(.by= c(DJ,ArtistToken), n = n()) |> 
  bind_tf_idf(ArtistToken, DJ, n)

distinctive_artists <- dj_tf_idf|> 
  slice_max(tf_idf, n = 100,by= DJ) |> 
  select(DJ, ArtistToken) |> 
  mutate(DJ = as.character(DJ))

# distinctive_artists
# save(distinctive_artists,file='data/distinctive_artists.rdata')
# save as parquet
cat("Saving distinctive_artists as parquet\n")
df_to_parquet(distinctive_artists, "data/distinctive_artists.parquet")


  

  
