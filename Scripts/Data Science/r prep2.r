# packages ---- 

# packages <- c("arm", "BMA", "brms", "corrplot", "dummies","DescTools", "estimatr","extrafont", "extrafontdb", "janitor",
#               "reshape2","tidyr","broom", "caret", "haven", "HH","Hmisc","lubridate","knitr", "margins", "magrittr", "plotrix",
#               "scales","survey", "srvyr", "sysfonts", "foreign","car", "ICC", "openxlsx", "ggrepel", "readr",
#               "readxl", "sjmisc", "sjPlot", "sjstats", "sjlabelled", "skimr","labelled", "texreg", "janitor","psych","dplyr",
#               "tidyverse", "viridis", "here", "ggridges", "ggthemes", "DT", "jtools", "huxtable", "stringi", "gghighlight",
#               "plm", "brms", "rstan", "rstanarm","tidybayes","texreg","gt","gtsummary","huxtable","stargazer", "gsynth",
#               "panelView", "assertr", "pointblank", "validate", "sandwich", "workflowr", "here", "missForest", "ltm")

# zam_packages <- c("tidyverse", "here", "gt","gtsummary","gghighlight","stringi","DT", "ggthemes","ggridges","viridis",
#               "psych","labelled","readxl", "skimr","sjlabelled","sjstats","sjPlot", "sjmisc","readr","ggrepel","openxlsx",
#               "sysfonts","scales","magrittr","knitr","lubridate","Hmisc","haven","tidyr","reshape2","DescTools","extrafont",
#               "extrafontdb")

# zam_packages2 <- c("tidyverse", "here", 
#                    "gt","gtsummary",
#                    "gghighlight","stringi","DT", "ggthemes","ggridges","viridis",
#                    "psych","labelled","readxl", "skimr","sjlabelled","sjstats","sjPlot", "sjmisc","readr","ggrepel","openxlsx",
#                    "sysfonts","scales","magrittr","knitr","lubridate","Hmisc","haven","tidyr","reshape2","DescTools","extrafont",
#                    "extrafontdb")

zam_packages <- c("tidyverse", "here", "gifski", "gstat","gt","gtsummary","gghighlight","stringi", "ggthemes","ggridges","viridis",
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
                  "FactoMineR", "osrm","spatialreg","RPostgres","devtools","terra","ragg","randomForest", "transformr", "tidygraph","tm","tibble","quanteda.textplots","spData")



# lapply(zam_packages, install.packages, character.only=T)

lapply(zam_packages, library, character.only=T)
#lapply(zam_packages2, library, character.only=T)

# font_import()
# loadfonts(device="win")
# windwsFonts()


# formatting ---- 

#font_add_google("Open Sans", "sans-serif")

options(digits=4, scipen=8)
#options(digits=8, scipen=9)

# set default
base <- theme_bw() + theme(panel.grid.minor.x=element_blank(),
                           panel.grid.minor.y=element_blank(),
                           plot.title=element_text(face="bold", size=14,hjust=.5, family = "Gill Sans Mt"),
                           plot.subtitle = element_text(size=13, family="Gill Sans Mt"),
                           plot.caption=element_text(size=13, family="Gill Sans Mt"),
                           axis.title=element_text(size=14, family="Gill Sans Mt"),
                           axis.text=element_text(size=13, family="Gill Sans Mt"),
                           axis.text.x = element_text(size = 10, family="Gill Sans Mt", face="bold"),
                           axis.text.y = element_text(size = 10, family="Gill Sans Mt", face="bold"),
                           legend.text=element_text(size=13, family="Gill Sans Mt"),
                           legend.position = "bottom",
                           # legend.position = c(.73,.99),
                           # legend.justification = c("left", "top"),
                           # legend.box.just = "left",
                           strip.text=element_text(size=11, family="Gill Sans Mt"),
                           panel.border=element_blank(),
                           axis.ticks = element_blank(),
                           legend.box="horizontal",
                           legend.background = element_rect(fill = "white", color = "black"))


baseX <- theme(plot.title = element_text(size = 15),
  plot.caption = element_text(size=12,family="Gill Sans Mt", face="bold"),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12,family="Gill Sans Mt", face="bold"),
  axis.text.x = element_text(size = 12, family="Gill Sans Mt", face="bold"),
  axis.text.y = element_text(size = 12, family="Gill Sans Mt", face="bold"),
  legend.text = element_text(size = 11),
  legend.title=element_blank(),
  legend.position="none",
  strip.text=element_text(size=12, family="Gill Sans Mt"))



baseC <- theme(plot.title = element_text(size = 15),
               plot.caption = element_text(size=12, hjust=0),
               axis.title.x = element_text(size = 10),
               axis.title.y = element_text(size = 10),
               axis.text.x = element_text(size = 9, family="Gill Sans Mt", face="bold"),
               axis.text.y = element_text(size = 12, family="Gill Sans Mt", face="bold"),
               legend.text = element_text(size = 11),
               legend.title=element_blank(),
               legend.position="none",
               strip.text=element_text(size=13, family="Gill Sans Mt"))





basey <- theme(plot.title = element_text(size = 16),
  plot.caption = element_text(size=10),
  axis.title.x = element_text(size = 14, family="Gill Sans Mt", face="bold"),
  axis.title.y = element_text(size = 14, family="Gill Sans Mt", face="bold"),
  axis.text.x = element_text(size = 14),
  axis.text.y = element_text(size = 14),
  legend.text = element_text(size = 14),
  legend.title=element_blank(),
  legend.position="right",
  strip.text=element_text(size=14, family="Gill Sans Mt"),
  legend.background = element_rect(fill = "white", color = "black"))


baseyNoGrid <- theme(
  plot.title = element_text(size = 16),
  plot.caption = element_text(size = 10),
  axis.title.x = element_text(size = 14, family = "Gill Sans Mt", face = "bold"),
  axis.title.y = element_text(size = 14, family = "Gill Sans Mt", face = "bold"),
  axis.text.x = element_text(size = 14),
  axis.text.y = element_text(size = 14),
  legend.text = element_text(size = 14),
  legend.title = element_blank(),
  legend.position = "right",
  strip.text = element_text(size = 14, family = "Gill Sans Mt"),
  legend.background = element_rect(fill = "white", color = "black"),
  panel.grid = element_blank(),
  panel.border = element_blank())

Angle <- theme(plot.title = element_text(size = 16),
               plot.caption = element_text(size=10),
               axis.title.x = element_text(size = 12, family="Gill Sans Mt", face="bold"),
               axis.title.y = element_text(size = 12, family="Gill Sans Mt", face="bold"),
               axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
               axis.text.y = element_text(size = 10),
               legend.text = element_text(size = 12),
               legend.title=element_blank(),
               legend.position="bottom",
               strip.text=element_text(size=14, family="Gill Sans Mt"),
               legend.background = element_rect(fill = "white", color = "black"))




non_base <- theme_bw() + theme(panel.grid.minor.x=element_blank(),
                           panel.grid.minor.y=element_blank(),
                           plot.title=element_text(face="bold",
                                                   size=16, 
                                                   hjust=.5, 
                                                   family = "Gill Sans Mt"),
                           plot.subtitle = element_text(size=12, family="Gill Sans Mt"),
                           plot.caption=element_text(size=12, family="Gill Sans Mt"),
                           axis.title=element_text(size=14, family="Gill Sans Mt"),
                           axis.text=element_text(size=13, family="Gill Sans Mt"),
                           axis.text.x = element_text(size = 11, family="Gill Sans Mt", face="bold"),
                           axis.text.y = element_text(size = 12, family="Gill Sans Mt", face="bold"),
                           legend.text=element_text(size=11, family="Gill Sans Mt"),
                           legend.position = "none",
                           # legend.position = c(.73,.99),
                           # legend.justification = c("left", "top"),
                           # legend.box.just = "left",
                           strip.text=element_text(size=13, family="Gill Sans Mt"),
                           panel.border=element_blank(),
                           axis.ticks = element_blank(),
                           # legend.box="horizontal",
                           legend.background = element_rect(fill = "white", color = "black"))


basem <- theme(plot.title = element_text(size = 15),
  plot.caption = element_text(size=10),
  axis.title.x = element_text(size = 9),
  axis.title.y = element_text(size = 9),
  axis.text.x = element_text(size = 9),
  axis.text.y = element_text(size = 9),
  legend.text = element_text(size = 9),
  legend.title=element_blank(),
  legend.position="bottom",
  strip.text=element_text(size=10, family="Gill Sans Mt"),
  legend.background = element_rect(fill = "white", color = "black"))







#scale_color_discrete <- usaid_palette

#opts <- options(ggplot2.discrete.color = usaid_palette)

theme_set(base)
# 
# faceted <- theme_bw() +
#   theme(panel.grid.minor.x=element_blank(),
#         panel.grid.minor.y=element_blank(),
#         plot.title=element_text(face="bold",
#           size=16, 
#           hjust=.5, 
#           family = "Gill Sans Mt"),
#         plot.subtitle = element_text(size=12, family="Gill Sans Mt"),
#         plot.caption=element_text(size=12, family="Gill Sans Mt"),
#         axis.title=element_text(size=12, family="Gill Sans Mt"),
#         axis.text=element_text(face="bold", size=10, family="Gill Sans Mt"),
#         legend.text=element_text(size=14, family="Gill Sans Mt"),
#         #legend.position = "left",
#         legend.position = c(.55,.99),
#         legend.justification = c("left", "top"),
#         legend.box.just = "left",
#         strip.text=element_text(size=12, family="Gill Sans Mt"))


faceted <- theme_bw() +
  theme(
        plot.title=element_text(face="bold",
                                size=16, 
                                hjust=.5, 
                                family = "Gill Sans Mt"),
        plot.subtitle = element_text(size=12, family="Gill Sans Mt"),
        plot.caption=element_text(size=12, family="Gill Sans Mt"),
        axis.title=element_text(size=12, family="Gill Sans Mt"),
        axis.text=element_text(face="bold", size=10, family="Gill Sans Mt"),
        legend.text=element_text(size=14, family="Gill Sans Mt"),
        #legend.position = "left",
        legend.position = c(.55,.99),
        legend.justification = c("left", "top"),
        legend.box.just = "left",
        strip.text=element_text(size=12, family="Gill Sans Mt"))


# USAID colors

usaid_blue <- "#002F6C"
usaid_red <- "#BA0C2F"
rich_black <- "#212721"
medium_blue <- "#0067B9"
light_blue <- "#A7C6ED"
web_blue <- "#205493"
dark_red <- "#651D32"
dark_grey <- "#6C6463"
medium_grey <- "#8C8985"
light_grey <- "#CFCDC9"

# ggtext strings

decred <- "<span style='color:#BA0C2F;'>**declining**</span>"
decblu <- "<span style='color:#002F6C;'>**declining**</span>"

increasblu <- "<span style='color:#002F6C;'>**increasing**</span>"
increasblu <- "<span style='color:#BA0C2F;'>**increasing**</span>"




usaid_palette <- c(web_blue, usaid_red, light_blue, dark_red, usaid_blue)
usaid_palette

usaid_palette6 <- c(web_blue
                    , usaid_red
                    , light_blue
                    , dark_red
                    , usaid_blue
                    , medium_grey)
scale_colour_discrete <- function(...) scale_colour_manual(..., values = usaid_palette)

# palette(usaid_palette)
# 
#  data(mtcars)
# # head(mtcars)
#  str(mtcars)
# frq(mtcars$carb)
# # 
#  ggplot(mtcars, aes(mpg, hp, color=as.factor(carb))) + 
#    geom_point() 
#  
#  +
#    scale_color_brewer(palette="Set2")
# #   
#   
#   scale_color_discrete()
# 
# ?scale_color_discrete
#   
#   
# 
#   + 
#   scale_color_manual(values=usaid_palette)

options(ggplot2.discrete.color = usaid_palette)

# Zambia colors

zamGreen <- "#198a00ff"
zamRed <- "#de2010ff"
zamOrange <- "#EF7D00"
zamBlack <- "#000000"

#Here's a USAID color scheme to apply to scale_color_manual()
#of all the plotting functions. To use it replace the existing color scale with
#scale_color_manual(values = colors, labels=get_labels(variable))

# colors = c("#002F6C", "#BA0C2F", "#0067B9", "#6C6463", "#651D32", "#A7C6ED", "#8C8985")



