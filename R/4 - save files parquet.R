library(arrow)
library(purrr)
library(glue)
library(ggplot2)

# copy precalculated data over to shiny app directory

#load(file = "data/dj_key.RData")
# load(file = "data/playlists.Rdata")
# load(file = "data/djSimilarity.RData")
# load(file = "data/distinctive_artists.RData")

# load(file = "all_artisttokens.rdata")

library(fs)

tables = c("dj_key", "playlists", "djsimilarity", "distinctive_artists")

file_ext = ".parquet"

##  save_parquet_to_local <- function(file_stem){
#  arrow::write_parquet(eval(parse(text=file_stem)),
#                       sink=paste0("data/",file_stem,file_ext))
#}

# fs::file_copy("data/dj_key.RData",
#              "../wfmu_explorer/data/dj_key.RData",
#              overwrite = TRUE)

copy_parquet_to_shiny <- function(table) {
  fs::file_copy(
    glue("data/{table}.parquet"),
    glue("../wfmu_explorer/data/{table}.parquet"),
    overwrite = TRUE
  )
}

save_parquet_to_shiny <- function(file_stem) {
  arrow::write_parquet(
    eval(parse(text = file_stem)),
    sink = paste0("../wfmu_explorer/data/", file_stem, file_ext)
  )
}

# tables |> walk(save_parquet_to_local)
tables |> walk(copy_parquet_to_shiny)
#tables |> walk(save_parquet_to_shiny)
#copy to other folder  "data/djdtm.RData" and "data/similarity_histogram_gg.rdata"
fs::file_copy(
  "data/djdtm.RData",
  "../wfmu_explorer/data/djdtm.RData",
  overwrite = TRUE
)
fs::file_copy(
  "data/similarity_histogram_gg.rdata",
  "../wfmu_explorer/data/similarity_histogram_gg.rdata",
  overwrite = TRUE
)
