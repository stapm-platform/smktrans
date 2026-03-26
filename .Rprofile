source("renv/activate.R")
# ==============================================================================
# This file runs each time the project is opened.
# Note: R package libraries are now managed entirely by `renv`. 
# Do NOT set .libPaths() here.
# ==============================================================================



# Create necessary project directories if they don't already exist 
# (Add any other output/data folders your scripts need here)
folder_paths <- c(
  "05_input",
  "outputs" # Adding this since you mentioned Excel outputs earlier!
)

# Safely build folders without throwing warnings if they already exist
for (fpath in folder_paths) {
  if (!dir.exists(fpath)) {
    dir.create(fpath, recursive = TRUE)
  }
}

# Clean up environment
rm(folder_paths, fpath)
