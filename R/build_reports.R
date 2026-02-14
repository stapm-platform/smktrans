#' Build Web Reports for Pkgdown
#'
#' @description Generates RMarkdown wrapper files in vignettes/articles.
#' These wrappers load the saved configuration and render the report body.
#' @export
build_web_reports <- function() {
  
  # 1. Define Paths
  article_dir <- "vignettes/articles"
  if (!dir.exists(article_dir)) dir.create(article_dir, recursive = TRUE)
  
  # Check for the configs file (we will save this in Step 3)
  config_path <- system.file("extdata", "report_configs.rds", package = "smktrans")
  if (config_path == "") {
    # Fallback for dev mode
    config_path <- "inst/extdata/report_configs.rds"
  }
  
  if (!file.exists(config_path)) {
    stop("Could not find inst/extdata/report_configs.rds. Did you save the configs in your master script?")
  }
  
  # Load configs to get the names
  all_configs <- readRDS(config_path)
  
  # 2. Loop through configs and create Rmd wrappers
  for (country in names(all_configs)) {
    
    message(paste(">> Creating Rmd Wrapper for:", country))
    
    # Filename: report_england.Rmd
    fname <- paste0("report_", tolower(gsub(" ", "_", country)), ".Rmd")
    fpath <- file.path(article_dir, fname)
    
    # The content of the wrapper Rmd
    # It loads the config list, extracts the right country, and includes the body template
    yaml_header <- paste0(
      "---\n",
      "title: \"Smoking Transition Estimates: ", country, "\"\n",
      "resource_files:\n",
      "  - ../../inst/extdata/report_configs.rds\n",
      "---\n\n"
    )
    
    r_chunk <- paste0(
      "```{r setup_wrapper, include=FALSE}\n",
      "# Load the config for this specific country\n",
      "configs <- readRDS(system.file('extdata', 'report_configs.rds', package = 'smktrans'))\n",
      "cfg <- configs[['", country, "']]\n",
      "```\n\n",
      "```{r child, child='../../inst/templates/web_report_body.Rmd'}\n",
      "```\n"
    )
    
    # Write the file
    writeLines(paste0(yaml_header, r_chunk), fpath)
    message(paste("   Generated:", fpath))
  }
  
}