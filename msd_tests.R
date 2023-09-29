library(RSQLite)
library(tidyverse)
library(dtplyr)
library(rhdf5)

#connect to a sqllite database on a local disk
fname <- list.files("data","^last.+")[1]
fname
file.exists("data/lastfm_tags.db")
file.exists(paste0("data/",fname))

con <- DBI::dbConnect(SQLite(),paste0("data/",fname))

con
dbListTables(con)
tags <- tbl(con,"tags") |> as_tibble()
tid_tag <- tbl(con,"tid_tag") |> as_tibble()
tids <- tbl(con,"tids") |> as_tibble()


query = "SELECT tags.tag, COUNT(tid_tag.tid) FROM tid_tag, tags WHERE tid_tag.tag=tags.ROWID GROUP BY tags.tag"
# submit sql query to database
temp <- dbGetQuery(con,query) |> as_tibble()

DBI::dbDisconnect(con)

DBI::dbDriver()

#install.packages("BiocManager")
#BiocManager::install("rhdf5")

# Call the R HDF5 Library
library("rhdf5")

# a function to remove columns with all zero values from a tibble
remove_zero_cols <- function(df){
  df %>% 
    select_if(~!all(.==0))
}

# Open the HDF5 file
# h5file <- H5Fopen("data/msd_summary_file.h5", "H5F_ACC_RDONLY")
rhdf5::h5ls("data/msd_summary_file.h5")
h5readAttributes("data/msd_summary_file.h5", "musicbrainz")
musicbrainz <- h5read("data/msd_summary_file.h5", "musicbrainz")

# get size of h5 group  
analysis <- h5read("data/msd_summary_file.h5", "analysis/songs") |> 
  as_tibble()

metadata <- h5read("data/msd_summary_file.h5", "metadata/songs") |> 
  as_tibble()

#close h5 file
h5closeAll()


analysis <- remove_zero_cols(analysis)
metadata <- remove_zero_cols(metadata)
