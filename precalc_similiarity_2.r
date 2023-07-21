#update doc term matrix and dj similarity table for saving to server
# use both song and artist

library(tm)
library(tidyverse)
library(lubridate)
library(word2vec)
library(vegan) #similarity measures

load("data/playlists.rdata")

#Analyze similarity 
#-------------------------------------------------------------  
# create embeddings
embed <- playlists |> select(ArtistToken,Title) |> 
  unique() |> 
  rownames_to_column(var = "song_id")

playlists <- playlists |> 
  left_join(embed)
#make sure there aren't extra levels
playlists$DJ<-factor(playlists$DJ,as.character(unique(playlists$DJ)))

playlists_concat<- tibble()
for (dj in levels(playlists$DJ)){
  print(dj)
  #put all words in string for each DJ
  playlists_concat<-bind_rows(playlists_concat,
                            tibble(
                              DJ=dj,
                              song_ids = playlists%>%
                                filter(DJ==dj)%>%
                                pull(song_id)%>% 
                                paste(collapse=" ")
                            ))
}


model <- playlists_concat$song_ids[1:5] |> word2vec()


#artists should not have factor levels
#concat_artists$Artists<-as.character(concat_artists$Artists)
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
djdtm<-DocumentTermMatrix(djCorpus) %>%removeSparseTerms(SPARSE)
save(djdtm,file="data/djdtm.rdata")
#-----------------------------------------------------------

dj_similarity<-djdtm %>%
  as.matrix() %>% 
  vegdist(method="bray") %>% 
  as.matrix()
#-----------------------------------------------------------

dj_similarity_tidy<-dj_similarity%>% 
as.data.frame() %>% 
mutate(DJ1=rownames(.)) %>% 
as_tibble() %>% 
gather(DJ2,Similarity,-DJ1) %>% 
mutate(Similarity=1-Similarity) %>% 
filter(DJ1 != DJ2) %>%  #remove diagonals
group_by(DJ1) %>% 
arrange(desc(Similarity))
save(dj_similarity_tidy,file='djsimilarity2.rdata')
