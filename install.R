# Run this once before launching the app to install all required packages.

cran_packages <- c(
  "shiny",
  "shinydashboard",
  "tidyverse",
  "survival",
  "survminer",
  "Formula",
  "rootSolve",
  "dlm",
  "matrixStats",
  "broom",
  "DT"
)

to_install <- cran_packages[!cran_packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
if (!requireNamespace("DepCens", quietly = TRUE)) {
  remotes::install_github("GabrielGrandemagne/DepCens")
}

message("All packages installed.")