# ==============================================================================
# Script: load_packages.R
# Purpose: Load all required dependencies for the smktrans project
# ==============================================================================

# 1. Core Data Manipulation & Utilities
library(data.table)
library(matrixStats)
library(stringr)
library(magrittr)

# 2. Reading & Writing Data
library(readxl)
library(openxlsx)

# 3. Stats & Testing
library(survey)
library(testthat)

# 4. Visualization & Reporting
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(viridis)
library(plotly)
library(DT)
library(crosstalk)
library(htmltools)
#library(flextable)
library(gt)
library(gtsummary)

# 5. STAPM Packages (Loaded LAST to ensure their functions take priority)
library(stapmr)
library(tobalcepi)
library(hseclean)
library(smktrans)
library(mort.tools)
library(toolkitr)

# Print warnings as they occur
options(warn = 1)

# Maximize data.table performance by using all available CPU cores
data.table::setDTthreads(0)
cat("data.table is using", getDTthreads(), "threads.\n")






