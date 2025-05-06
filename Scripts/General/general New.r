source("scripts/r prep2.r")
library(gt)
gtcars
mtcars 

data()
head(mtcars)
df %>%gt() %>%
  tab_header(title = "mtcars dataset") %>%
  tab_style(
    style = list(cell_fill(color = "#b2f7ef"),
                           cell_text(weight = "bold")),
                 locations = cells_body(columns = mpg))%>%
      tab_style(
        style = list(cell_fill(color = "#ffefb5"),
                               cell_text(weight = "bold")), 
                     locations = cells_body(columns = hp))

install.packages("formattable")
library(formattable)

formattable(gtcars, list(
  hp = color_bar("#e9c46a"),
  bdy_style = color_bar("#80ed99"),
  trim = color_bar("#48cae4"),
  hp_rpm = color_bar("#f28482")))



remotes::install_github("kcuilla/reactablefmtr")
library(reactablefmtr)
head(mtcars)

##Square Bars
reactable(gtcars,defaultColDef = colDef(
  cell = data_bars(gtcars,text_position = "outside-base")
))

##Circled Bars

library(reactablefmtr)
reactable(gtcars,defaultColDef = colDef(cell = data_bars(gtcars, box_shadow = TRUE, round_edges = TRUE,
                                                       text_position = "outside-base",
                                                       fill_color = c("#e81cff", "#40c9ff"),
                                                                       background = "#e5e5e5",fill_gradient = TRUE)
                                                       ))
