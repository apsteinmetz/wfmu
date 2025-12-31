#update doc term matrix and dj similarity table for saving to server

library(tm)
library(tidyverse)
library(lubridate)
library(tidytext)
library(vegan) #similarity measures
library(duckplyr)
# load("data/playlists.rdata")
# load playlists with duckplyr
playlists <- read_parquet_duckdb("data/playlists.parquet") |>
  collect()
methods_restore()

#Analyze similarity
#-------------------------------------------------------------
cat(
  "Building recency-weighted Artist+Title tf-idf matrix and computing cosine similarity\n"
)

# parameters — adjust as needed
half_life_days <- 365 # half-life for recency decay (days)
artist_weight <- 1.5 # relative weight for artist token
title_weight <- 1.0 # relative weight for title tokens
min_title_token_length <- 3

# reference date for recency (use latest AirDate in data)
ref_date <- max(playlists$AirDate, na.rm = TRUE)

decay_fn <- function(age_days, half_life) {
  2^(-age_days / half_life)
}

playlists_rec <- playlists |>
  mutate(
    AirDate = as.Date(AirDate),
    age_days = as.numeric(ref_date - AirDate),
    recency_w = decay_fn(age_days, half_life_days)
  )

# artist tokens (keep ArtistToken as whole token, sanitize for column names)
artist_terms <- playlists_rec |>
  filter(!is.na(ArtistToken), ArtistToken != "") |>
  filter(!str_detect(ArtistToken, '^[0-9]+$')) |>
  transmute(
    DJ,
    term = paste0(
      "artist_",
      str_replace_all(ArtistToken, "[^A-Za-z0-9]+", "_")
    ),
    weight = recency_w * artist_weight
  )

# title tokens — treat the whole Title as a single token (sanitized, lowercased)
title_terms <- playlists_rec |>
  filter(!is.na(Title), Title != "") |>
  transmute(
    DJ,
    # lowercase, replace non-alphanumerics with underscore, collapse repeats, trim edge underscores
    title_clean = str_to_lower(Title) |>
      str_replace_all("[^A-Za-z0-9]+", "_") |>
      str_replace_all("_+", "_") |>
      str_replace_all("^_|_$", ""),
    recency_w
  ) |>
  filter(str_length(title_clean) >= min_title_token_length) |>
  transmute(
    DJ,
    term = paste0("title_", title_clean),
    weight = recency_w * title_weight
  )

# combine and aggregate weighted counts per DJ-term
terms_long <- bind_rows(artist_terms, title_terms) |>
  group_by(DJ, term) |>
  summarize(weight_n = sum(weight), .groups = "drop")

# compute tf-idf using weighted counts (bind_tf_idf accepts the count column)
dj_tf_idf_combined <- terms_long |>
  bind_tf_idf(term, DJ, weight_n)

# pivot to wide matrix (DJs x terms) using tf-idf values
dtm_wide <- dj_tf_idf_combined |>
  select(DJ, term, tf_idf) |>
  pivot_wider(names_from = term, values_from = tf_idf, values_fill = 0) |>
  arrange(DJ)

mat <- as.matrix(dtm_wide[, -1])
rownames(mat) <- dtm_wide$DJ

# compute cosine similarity matrix
row_norms <- sqrt(rowSums(mat * mat))
row_norms[row_norms == 0] <- 1
mat_norm <- mat / row_norms
sim_mat <- mat_norm %*% t(mat_norm)
sim_mat[is.na(sim_mat)] <- 0

# tidy similarity table (exclude self-similarity)
dj_similarity_tidy <- as_tibble(sim_mat, rownames = "DJ1") |>
  pivot_longer(-DJ1, names_to = "DJ2", values_to = "Similarity") |>
  filter(DJ1 != DJ2) |>
  group_by(DJ1) |>
  arrange(desc(Similarity)) |>
  ungroup()

# save results
cat("Saving djsimilarity as parquet\n")
compute_parquet(dj_similarity_tidy, "data/djsimilarity.parquet")

# what artists make a dj different from another
cat("Computing distinctive artists\n")
dj_tf_idf <- playlists |>
  select(DJ, ArtistToken) |>
  filter(ArtistToken != '') |>
  # remove ArtistToken where only numerals, probably bogus
  filter(!str_detect(ArtistToken, '^[0-9]+$')) |>
  summarise(.by = c(DJ, ArtistToken), n = n()) |>
  bind_tf_idf(ArtistToken, DJ, n)

distinctive_artists <- dj_tf_idf |>
  slice_max(tf_idf, n = 100, by = DJ) |>
  select(DJ, ArtistToken) |>
  mutate(DJ = as.character(DJ))

# distinctive_artists
# save(distinctive_artists,file='data/distinctive_artists.rdata')
# save as parquet
cat("Saving distinctive_artists as parquet\n")
compute_parquet(distinctive_artists, "data/distinctive_artists.parquet")
