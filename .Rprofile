
## This file will run each time the project is opened, and ensures the 
## R packages are read from the correct local project library and sets up 
## the sub-folders for the project.

####################################################
### Create directories if they don't already exist 
folder_paths <- c("00_R_env",
                  "00_R_env/_R_packages")

sapply(folder_paths, function(fpath) {
  outputDir <- fpath
  if(!dir.exists(here::here(outputDir))) {dir.create(here::here(outputDir))}
})

rm(folder_paths)

####################################################
#### Set the project library

project_lib <- "00_R_env/_R_packages"

.libPaths(project_lib)

