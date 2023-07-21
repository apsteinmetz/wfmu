# copy precalculated data over to shiny app directory

# load then save with old file format that works with R 3.4

load(file = "data/DJKey.RData")
load(file = "data/playlists.Rdata")
load(file = "data/djSimilarity.RData")
load(file = "data/djdtm.RData")

save(DJKey,file = "data/DJKey.RData",version = 2)
save(playlists,file = "data/playlists.Rdata",version = 2)
save(dj_similarity_tidy,file = "data/djSimilarity.RData",version = 2)
save(djdtm,file = "data/djdtm.RData",version = 2)





library(fs)

files = c("DJKey.rdata",
          "djdtm.rdata",
          "playlists.rdata",
          "djsimilarity.rdata")
shiny_app_dir = "../wfmu_explorer/"
file_copy(paste0("data/",files),paste0(shiny_app_dir,files),overwrite = TRUE)
