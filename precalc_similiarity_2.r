# this was a failed experiment.  word2vec did not do better
# than a document term matrix using artist only
# using artist and song was too small a set of commonality
# use embeddings and word2vec to get similarity measures
# use both song and artist
library(tidyverse)
library(word2vec)
library(doc2vec)

load("data/playlists.rdata")

#Analyze similarity 
#-------------------------------------------------------------  
# create embeddings
# embed <- playlists |> select(ArtistToken) |> 
#   # embed <- playlists |> select(ArtistToken,Title) |> 
#   unique() |> 
#   rownames_to_column(var = "token_id")
# 
# playlists <- playlists |> 
#   left_join(embed)

playlists <- playlists |>
  mutate(token_id = str_replace_all(ArtistToken,
                                    " ", "_")) |>
  filter(token_id != "")

#make sure there aren't extra levels
playlists$DJ<-factor(playlists$DJ,as.character(unique(playlists$DJ)))
# take top 999 songs for each DJ. Limitation of doc2vec



tokens_to_keep <- playlists |> 
  select(DJ,token_id) |> 
  group_by(DJ,token_id) |> 
  count(token_id) |> 
  filter(n > 2)
  
tokens_by_freq <- 
  tokens_to_keep |> 
  left_join(playlists,by = c("DJ","token_id")) |> 
  group_by(DJ) |> 
  #  sample with replacement to fill DJs with less than 
  # 1000 songs played at least twice
  slice_sample(n = 999,replace = TRUE) |> 
  select(DJ,token_id)

  
db <- tibble()
for (dj in levels(tokens_by_freq$DJ)) {
  print(dj)
  #put all words in string for each DJ
  db <- bind_rows(db,
                  tibble(
                    doc_id = dj,
                    text = tokens_by_freq %>%
                      filter(DJ == dj) %>%
                      pull(token_id) %>%
                      paste(collapse = " ")
                  ))
}

model <- paragraph2vec(db,dim = 150,min_count = 2)

similar = predict(model,newdata = "CF",
                  type = "nearest",
                  which = "doc2doc",
                  top_n = 20)




similar |>  
  bind_rows() |> 
  rename(DJ = term2) |> 
  left_join(DJKey) |> 
  select(1:5)
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
