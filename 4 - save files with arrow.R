library(arrow)
library(purrr)

# copy precalculated data over to shiny app directory

load(file = "data/DJKey.RData")
load(file = "data/playlists.Rdata")
load(file = "data/djSimilarity.RData")
load(file = "data/djdtm.RData")


library(fs)

tables = c("DJKey",
          "playlists",
          "dj_similarity_tidy")

file_ext = ".parquet"

save_parquet <- function(file_stem){
  arrow::write_parquet(eval(parse(text=file_stem)),
                       sink=paste0("data/",file_stem,file_ext))
}

save_parquet2 <- function(file_stem){
  arrow::write_parquet(eval(parse(text=file_stem)),
                       sink=paste0("../wfmu_explorer/data/",file_stem,file_ext))
}

tables |> walk(save_parquet2)
save(djdtm,file="../wfmu_explorer/data/djdtm.rdata")
