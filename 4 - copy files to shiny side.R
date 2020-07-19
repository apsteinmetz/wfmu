# copy precalculated data over to shiny app directory

# load then save with old file format that works with R 3.4

load(file='DJKey.RData')
load(file="playlists.Rdata")
load(file='djSimilarity.RData')
load(file='djdtm.RData')

save(DJKey,file='DJKey.RData',version = 2)
save(playlists,file="playlists.Rdata",version = 2)
save(dj_similarity_tidy,file='djSimilarity.RData',version = 2)
save(djdtm,file='djdtm.RData',version = 2)





library(fs)

files = c("DJKey.rdata",
          "djdtm.rdata",
          "playlists.rdata",
          "djsimilarity.rdata")
shiny_app_dir = "../wfmu_explorer/"
file_copy(files,paste0(shiny_app_dir,files),overwrite = TRUE)
