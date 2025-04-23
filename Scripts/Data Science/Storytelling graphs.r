source("scripts/r prep2.r")


#Load survey data
path_plot <- read_xlsx("Data/general/survey data.xlsx")

path_plot

#get today’s date
current_date <- Sys.Date()

# load libraries
library(tidyverse)
library(data.table)

d <- setDT(data.frame(
  item_1 = sample(1:4,50,replace = TRUE),
  item_2 = sample(2:5,50,replace = TRUE),
  item_3 = sample(1:3,50,replace = TRUE),
  item_4 = sample(1:5,50,replace = TRUE),
  item_5 = sample(1:3,50,replace = TRUE),
  item_6 = sample(4:5,50,replace = TRUE),
  item_7 = sample(1:5,50,replace = TRUE)
))

# change colnames to items
colnames(d) <-c("Christmas has become only another occasion for excessive consumption.",
                 "Being with my family is the thing I enjoy most about Christmas.",
                 "Christmas does not really matter to me.",
                 "If I needed to choose, Christmas would be my absolute favourite holiday.",
                 "The amount of stress around Christmas exceeds the amount of joy to me.",
                 "I could not imagine to abolish Christmas as a national holiday.",
                 "Christmas has religious meaning to me.")

#transform the dataframe to a long format
d_long <- d %>%
  gather(key = "names", value ="value", factor_key = TRUE)

# predefine formatting function for long items
get_wraper <- function(width) {
  function(x) {
    lapply(strwrap(x, width = width, simplify = FALSE), paste, collapse="\n")
  }
}

# set basics
barplot <- ggplot(d_long,aes(reorder(names,value), value, fill = names, col = names, group = names))+
  # calculate mean for each item and represent in bars
  stat_summary(fun = mean, geom = "bar", color = "white") +
  # add point to represent point-estimate-nature
  stat_summary(fun = mean, geom = "point", col = "black")+
  # fill bars with nicer colors
  scale_fill_brewer(palette = "Dark2")+
  # add errorbars
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width = 0.1, col = "black")+
  # add comprehensive title
  ggtitle("Attitudes towards Christmas in Decembre 2020", subtitle = "Points = Mean, Whiskers = SD")+
  # add y-label
  ylab("Responses")+
  # use custom-made wrapper function to fix long sentences - remember that axis are flipped!
  scale_x_discrete(labels = get_wraper(30))+
  # align bars to proper label 
  scale_y_continuous(expand=c(0,0), limits = c(0, 6))+
  # add classic theme as a base
  # flip axis to make bars horizontal
  coord_flip()+
  theme_classic()+
  # add formatting
  theme(axis.title.y= element_blank(), axis.text.x=element_text(size=12),
        legend.position = "none", axis.text.y=element_text(size = 12),
        plot.title = element_text(color = "black", size = 16, face = "bold"))

barplot

ggsave("viz/RTC/Likert_Christmas_Survey.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 15)

#ggsave(barplot, file=paste0(current_date,"_","Likert_Christmas_Survey.png"), path=paste0(path_plot),width = 30, height = 15, units = "cm")

#a custom-made plot
# Define positions
posn.d <- position_dodge(width = 0.1)
posn.jd <- position_jitterdodge(jitter.width = 0.1, dodge.width = 0.2)
posn.j <- position_jitter(width = 0.2)

# Function to save range for use in ggplot
gg_range <- function(x) {
  # Change x below to return the instructed values
  data.frame(ymin = min(x), # Min
             ymax = max(x)) # Max
}

# Function to save IQR around the median
med_IQR <- function(x) {
  # Change x below to return the instructed values
  data.frame(y = median(x), # Median
             ymin = quantile(x)[2], # 1st quartile
             ymax = quantile(x)[4]) # 3rd quartile
}
# set basics
coolplot <- ggplot(d_long, aes(reorder(names,value), value, col = names, fill = names))+
  # add jitter in the background
  geom_jitter()+
  # add reference line
  geom_hline(yintercept = 3, colour = "grey")+
  # add interquartile range
  stat_summary(geom = "linerange", fun.data = med_IQR,
               position = posn.d, size = 6) +
  # add total range (more transparent)
  stat_summary(geom = "linerange", fun.data = gg_range,
               position = posn.d, size = 6,
               alpha = 0.4) +
  # add median represented by X
  stat_summary(geom = "point", fun = median,
               position = posn.d, size = 6,
               col = "black", shape = "X")+
  # add colour
  scale_color_brewer(palette = "Dark2")+
  # set title (ggtitle alternative)
  labs( title = "Attitudes towards Christmas in Decembre 2020", subtitle = "X = Median, colored Box = 75% of responses, shadow = range")+
  # set y-label
  ylab("Responses")+
  # set y-label (later x-label)
  scale_y_continuous(limits = c(0,6), breaks = seq(1, 5, by = 1))+
  # set x-label (format long sentences)
  scale_x_discrete(labels = get_wraper(30))+
  # flip axis to make it horizontal
  coord_flip()+
  # set theme as a base
  theme_classic(base_size = 12)+
  # add more formatting
  theme(axis.title.y= element_blank(), 
        panel.background = element_rect(fill = "white", color = "black"),
        legend.position = "none",
        plot.title = element_text(color = "black", size = 16, face = "bold"))

coolplot

ggsave("viz/RTC/Likert_Christmas_Survey2.png",
       device="png",
       type="cairo",
       height = 6.5,
       width = 15)
#ggsave(coolplot, file=paste0(current_date,"_","Customplot_Christmas_Survey.png"), path=paste0(path_plot),width = 30, height = 15, units = "cm")

#back-to-back divergent bar-chart
#remotes::install_github('jbryer/likert')
library(likert)
# correct for naturally occuring unequal number of factor levels in the data
data_l <- d %>% 
  mutate_all(
    funs(factor(case_when(
      . == 1 ~ "Strongly disagree",
      . == 2 ~ "Disagree",
      . == 3 ~ "Neutral",
      . == 4 ~ "Agree",
      . == 5 ~ "Strongly agree"),
      levels = c("Strongly disagree","Disagree","Neutral","Agree", "Strongly agree")
    )))

likertplot <- plot(likert::likert(data_l),low.color = "#FF6665", high.color = "#5AB4AC", neutral.color.ramp = "white", 
                   neutral.color = "grey90")+
  ggtitle("Attitudes towards Christmas in Decembre 2020")+
  theme_classic(base_size = 12)

likertplot

ggsave(likertplot, file=paste0(current_date,"_","Likertplot_Christmas_Survey.png"), path=paste0(path_plot),width = 30, height = 15, units = "cm")

