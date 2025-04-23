# Load or install remotes first
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Master list of CRAN packages
zam_cran_packages <- unique(c(
  "tidyverse", "here", "gifski", "gstat", "gt", "gtsummary", "gghighlight", "stringi", "ggthemes", 
  "ggridges", "viridis", "psych", "labelled", "readxl", "skimr", "sjlabelled", "sjstats", "sjPlot", 
  "sjmisc", "readr", "ggrepel", "openxlsx", "sysfonts", "scales", "magrittr", "knitr", "lubridate", 
  "Hmisc", "haven", "tidyr", "reshape2", "DescTools", "extrafont", "patchwork", "extrafontdb", 
  "ggtext", "geomtextpath", "zoo", "cowplot", "ztable", "pheatmap", "RColorBrewer", "data.table", 
  "d3heatmap", "hablar", "gganimate", "dygraphs", "gapminder", "hrbrthemes", "gridExtra", "grid", 
  "rmarkdown", "forecast", "backtest", "quantmod", "tseries", "writexl", "ggpubr", "rcartocolor", 
  "geodata", "shiny", "rgeoboundaries", "nasapower", "classInt", "spData", "tmap", "tmaptools", 
  "spDataLarge", "leaflet", "ggsflabel", "sf", "RSelenium", "netstat", "GGally", "calendR", 
  "areaplot", "hexbin", "webshot", "oceanis", "spdep", "gsubfn", "proto", "fpp", "mapview", 
  "shinydashboard", "packrat", "formattable", "reactablefmtr", "caret", "predict3d", "ggpmisc", 
  "googleway", "ggplot2", "caTools", "giscoR", "ggspatial", "sp", "glmnet", "rnaturalearth", 
  "rnaturalearthdata", "pacman", "vroom", "rgdal", "ggraph", "igraph", "Matrix", "network", 
  "quanteda", "sna", "maps", "RODBC", "elevatr", "cartography", "Cairo", "sfdep", "terra", 
  "likert", "sendmailR", "DBI", "RPostgres", "janitor", "reticulate", "DT", "mice", 
  "FactoMineR", "osrm", "spatialreg", "transformr", "tidygraph", "tm", "tibble", "quanteda.textplots"
))

# GitHub packages to install (manually specified)
github_packages <- c(
  "datalorax/ggsflabel",       # example of GitHub-only package
  "hrbrmstr/hrbrthemes",       # occasionally more current on GitHub
  "rstudio/gt",                # for development version of gt
  "thomasp85/gganimate",       # dev version for animated plots
  "r-spatial/sfdep"            # newer versions if needed
)

# Install missing CRAN packages
missing_cran <- setdiff(zam_cran_packages, installed.packages()[, "Package"])
if (length(missing_cran)) {
  install.packages(missing_cran)
} else {
  message("All CRAN packages are already installed.")
}

# Install GitHub packages
for (pkg in github_packages) {
  pkg_name <- sub(".*/", "", pkg) # extract package name
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    remotes::install_github(pkg)
  }
}
