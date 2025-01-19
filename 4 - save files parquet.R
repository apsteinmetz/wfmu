library(arrow)
library(purrr)
library(glue)

# copy precalculated data over to shiny app directory

#load(file = "data/djKey.RData")
# load(file = "data/playlists.Rdata")
# load(file = "data/djSimilarity.RData")
# load(file = "data/distinctive_artists.RData")
load(file = "data/djdtm.RData")
# load(file = "all_artisttokens.rdata")


library(fs)

tables = c("djKey",
          "playlists",
          "dj_similarity_tidy",
          "distinctive_artists")

file_ext = ".parquet"

##  save_parquet_to_local <- function(file_stem){
#  arrow::write_parquet(eval(parse(text=file_stem)),
#                       sink=paste0("data/",file_stem,file_ext))
#}

fs::file_copy("data/djKey.RData",
              "../wfmu_explorer/data/djKey.RData",
              overwrite = TRUE)



copy_parquet_to_shiny <- function(table){
  fs::file_copy(glue("data/{table}.parquet"),
                glue("../wfmu_explorer/data/{table}.parquet"),
                overwrite = TRUE)
}

save_parquet_to_shiny <- function(file_stem){
  arrow::write_parquet(eval(parse(text=file_stem)),
                       sink=paste0("../wfmu_explorer/data/",file_stem,file_ext))
}

# tables |> walk(save_parquet_to_local)
tables |> walk(copy_parquet_to_shiny)
#tables |> walk(save_parquet_to_shiny)
save(djdtm,file="../wfmu_explorer/data/djdtm.rdata")
# save(all_artisttokens,file="../wfmu_explorer/data/all_artisttokens.rdata")

