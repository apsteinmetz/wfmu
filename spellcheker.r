#spell checker functions
# from https://github.com/sharan-naribole/H1B_visa_eda/blob/master/data_processing.Rmd

library(hashmap)

get_inserts <- function(split_left,split_right, i, letters) {
  # Generate insertions of a single letter
  return(unlist(sapply(letters, function(left,right,c) {return(paste0(left, c, right))}, left = split_left[i], right = split_right[i])))
}

get_deletes <- function(split_left,split_right, i) {
  # Generate deletion of one letter from word
  return(paste0(split_left[i], substr(split_right[i],2,nchar(split_right[i]))))
}

get_replaces <- function(split_left,split_right, i,letters) {
  # Generate replacement of a letter by a-z or space
  if(!is.null(split_right[i]) &  nchar(split_right[i]) > 0) {
    return(unlist(sapply(letters, function(left,right,c) {return(paste0(left, c, right))}, left = split_left[i], right = substr(split_right[i],2,nchar(split_right[i])))))
  }
  return(NULL)
}

get_transposes <- function(split_left, split_right,i) {
  # Generate interchanging of the positions of adjacent letters
  if(!is.null(split_right[i]) & nchar(split_right[i]) > 1) {
    return(paste0(split_left[i],substr(split_right[i],2,2),substr(split_right[i],1,1),substr(split_right[i],3,nchar(split_right[i]))))
  }
  return(NULL)
}

edits1artist <- function(artist) {
  # All edits that are one edit away from artist
  letters = toupper(strsplit("abcdefghijklmnopqrstuvwxyz ",split='')[[1]])
  artist_len <- nchar(artist)
  #print(artist_len)
  if(artist_len < 4) {
    return(artist)
  }
  split_left <- sapply(seq(0,artist_len), substr,x = artist,start = 1)
  split_right <- sapply(seq(1,artist_len+1), substr,x = artist,stop = artist_len) 
  deletes <- sapply(seq(1,artist_len+1),get_deletes, split_left = split_left, split_right = split_right)
  transposes <- unlist(sapply(seq(1,artist_len+1),get_transposes, split_left = split_left, split_right = split_right))
  replaces <- unlist(sapply(seq(1,artist_len+1),get_replaces, split_left = split_left, split_right = split_right, letters=letters))
  inserts <- unlist(sapply(seq(1,artist_len+1),get_inserts, split_left = split_left, split_right = split_right,letters = letters))
  
  return(unique(c(deletes,transposes,replaces,inserts)))
}

edits2artist <- function(artist) { 
  # All edits that are two edits away from `word`
  edits1_artists = edits1artist(artist)
  return (unlist(sapply(edits1_artists, edits1artist)))
}

get_prob <- function(artist, artist_hash) {
  # probability of artist in our dataset
  return(artist_hash[[artist]])
}

known <- function(artists,artist_hash = artist_hash) {
  # The subset of candidate artists that appear in the dictionary of artists
  return(artists[artist_hash$has_keys(artists)])
}

find_candidates <- function(artist,...) {
  # Generate possible spelling corrections for word
  return(c(known(artist,...), known(edits1artist(artist),...), c(artist)))
}

artist_spell_correcter <- function(artist,...) {
  # best possible correction to the artist
  candidates = find_candidates(artist,...)
  best_candi = candidates[which.max(sapply(candidates,get_prob, ...))]
  
  #if(get_prob(best_candi,...) > get_prob(artist,...) ) {
  #  return(best_candi)
  #}
  return(best_candi)
}

artists_count <- function(artist, artist_hash) {
  
  if(artist_hash$has_key(artist)) {
    return(artist_hash[[artist]])
  }
  return(artist)
}

artist_correct <- function(x, hash) {
  if(hash$has_key(x)) {
    return(hash[[x]])
  }
  return(x)
}

# MAIN ------------------------------------------------------------

fix_spelling<- function(artist_vector){
  temp<-as.data.frame(table(artist_vector))
  names(temp)<-c('artist','count')
  temp%>%arrange(desc(count))->artist_count
  
  artist_hash = hashmap(artist_count$artist, artist_count$count)
  
  artists <- artist_count$artist
  artists_before <- c()
  artists_after <- c()
  count <- 0
  
  for(artist in artists) {
    # Count of current Workartist
    curr_count <- artists_count(artist,artist_hash)
    #print(paste0(artist, ", ",curr_count))
    
    if(curr_count < 100) { # Threshold
      #print(paste0(artist, ", ",curr_count))
      corrected <- artist_spell_correcter(artist,artist_hash)
      
      if(corrected != artist) { # Correction occurred
        count <- count + 1
        artists_before[count] <- artist
        artists_after[count] <- corrected
        corrected_count <- artists_count(corrected,artist_hash)
        #print(paste0(artist, " : ", curr_count,", ",corrected, " : ", corrected_count))
      }
    }  
  }
  
  artists_corrected_hash <- hashmap(artists_before,artists_after)
  print(paste0("Number of workartist spelling corrections: ", length(artists_after)))
  return(artist_correct(artist_vector,artists_corrected_hash))
}
