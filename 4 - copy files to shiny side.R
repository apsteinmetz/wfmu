# copy precalculated data over to shiny app directory
library(fs)

files = c("DJKey.rdata",
          "djdtm.rdata",
          "playlists.rdata",
          "djsimilarity.rdata")
shiny_app_dir = "../wfmu_explorer/"
file_copy(files,paste0(shiny_app_dir,files),overwrite = TRUE)
