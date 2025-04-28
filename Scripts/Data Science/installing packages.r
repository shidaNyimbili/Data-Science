# Load or install remotes first
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Master list of CRAN packages
zam_cran_packages <- unique(c(
  "tidyverse", "here", "gifski", "gstat","gt","gtsummary","gghighlight","stringi", "ggthemes","ggridges","viridis",
  "psych","labelled","readxl", "skimr","sjlabelled","sjstats","sjPlot", "sjmisc","readr","ggrepel","openxlsx",
  "sysfonts","scales","magrittr","knitr","lubridate","Hmisc","haven","tidyr","reshape2","DescTools","extrafont",
  "patchwork", "extrafontdb", "ggtext", "geomtextpath","zoo","psych","cowplot","ztable","pheatmap",
  "RColorBrewer", "data.table", "d3heatmap","pheatmap","hablar", "gganimate", "dygraphs", "gapminder", "hrbrthemes",
  "geomtextpath", "patchwork", "gridExtra", "grid", "rmarkdown", "forecast", 
  "backtest", "quantmod", "tseries", "writexl", "ggpubr", "rcartocolor", "remotes", "geodata",
  "shiny", "rgeoboundaries", "nasapower", "classInt","spData","tmap","tmaptools","spDataLarge",
  "leaflet", "ggsflabel", "sf", "RSelenium", "netstat", "GGally", "calendR", "areaplot", "hexbin",
  "webshot", "oceanis", "SPlit", "spdep","gsubfn", "proto","fpp", "mapview", "shinydashboard", "packrat",
  "gt", "bigD","formattable", "reactablefmtr", "scales", "caret","predict3d", "ggpmisc", "cowplot", 
  "googleway", "ggplot2", "caTools","ggrepel", "giscoR", "ggspatial", "sp", "glmnet", "rnaturalearth", 
  "rnaturalearthdata", "pacman", "vroom", "rgdal","gridExtra", "GGally","ggraph","igraph","Matrix","network",
  "quanteda", "sna","maps", "RODBC","elevatr", "cartography","Cairo","sfdep","spdep","igraph", 
  "rnaturalearth", "terra","likert","sendmailR","DBI","RPostgres","data.table","janitor","reticulate", "DT", "mice",
  "FactoMineR", "osrm","spatialreg","RPostgres","devtools","terra","ragg","randomForest", "transformr", "tidygraph","tm","tibble","quanteda.textplots","spData"
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

library(chattr)

##Others

remotes::install_github("mlverse/chattr", force = TRUE)

# Install remotes package if you don't have it
install.packages("remotes")

# Then install from GitHub
remotes::install_github("wmgeolab/rgeoboundaries")
# spDataLarge is stored on GitHub
remotes::install_github("Nowosad/spDataLarge")

# ggsflabel also only available via GitHub
remotes::install_github("yutannihilation/ggsflabel")
install.packages("fpp2")

