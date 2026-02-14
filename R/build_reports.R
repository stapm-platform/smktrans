#' Build Web Reports for Pkgdown
#'
#' @description Generates HTML articles for the package website based on the
#' configuration lists used in the analysis.
#' @param config_list A list of configuration objects (e.g. list(config_eng, config_scot))
#' @export
build_web_reports <- function(config_list) {
  
  options(rmarkdown.html_vignette.check_title = FALSE)
  
  # 1. Ensure Directory Exists
  # pkgdown looks for "vignettes/" or "vignettes/articles/"
  article_dir <- "vignettes/articles"
  if (!dir.exists(article_dir)) dir.create(article_dir, recursive = TRUE)
  
  # 2. Locate Template
  # We assume the file is in inst/templates
  # When the package is loaded, system.file finds it. 
  # If running locally (dev), we look in relative path.
  tmpl <- system.file("templates", "web_report.Rmd", package = "smktrans")
  if (tmpl == "") tmpl <- "inst/templates/web_report.Rmd"
  
  if (!file.exists(tmpl)) stop("Could not find inst/templates/web_report.Rmd")
  
  # 3. Loop through configs
  for (cfg in config_list) {
    
    country_name <- cfg$country
    message(paste(">> Rendering Web Report for:", country_name))
    
    # Output filename (pkgdown friendly: lowercase, no spaces)
    out_name <- paste0("report_", tolower(gsub(" ", "_", country_name)), ".html")
    out_path <- file.path(article_dir, out_name)
    
    # Render
    rmarkdown::render(
      input = tmpl,
      output_file = out_name,
      output_dir = article_dir,
      params = list(config = cfg),
      quiet = TRUE
    )
    
    message(paste("   Created:", out_path))
  }
  
}