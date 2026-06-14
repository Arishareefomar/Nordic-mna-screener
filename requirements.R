# requirements.R
# Run this script once before running NM_A.R or knitting the report.

packages <- c(
  "readxl",
  "dplyr",
  "janitor",
  "stringr",
  "readr",
  "ggplot2",
  "knitr"
)

installed <- rownames(installed.packages())

for (pkg in packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg)
  }
}

message("All packages installed.")