
# library("tidyverse")
# library("lubridate")

#install.packages("vroom")

source("scripts/dataPrep.r")

tdb_data <- read_csv("Data/IHM/Stata4.csv")

unique(tdb_data)

glimpse(tdb_data)

tdb_data.r <- tdb_data %>%
  select(province, facility_name, tdb_creation_date, user_ips, most_recent_interaction_date)

tdb_data.r <- tdb_data.r %>%
  filter(complete.cases(.))

#tdb_data.r$creation_date <- mdy(tdb_data.r$tdb_creation_date)

tdb_data.r

tdb_data2 <- tdb_data.r %>%
  mutate(province=as.factor(province), facility_name=as.factor(facility_name), user_ips=as.factor(user_ips), 
         tdb_creation_date=as.Date(tdb_creation_date, format = "%m/%d/%Y"), 
         most_recent_interaction_date=as.Date(most_recent_interaction_date, format = "%m/%d/%Y"))

tdb_data

tdb_data2 <- tdb_data2 %>%
  separate(most_recent_interaction_date, into = c("Year","Month", "Day"))


tdb_data3 <- tdb_data2 %>%
  mutate(Month = month.abb[as.numeric(Month)])
tdb_data3

tdb_data3$Date <- str_c(tdb_data3$Year, tdb_data3$Month, tdb_data3$Day, sep = "-")

tdb_data3

tdb_data3 <- tdb_data3 %>%
  mutate(Month=as.factor(Month))
tdb_data3


# tdb_data.r <- tdb_data.r %>%
#   mutate(month_tdb_created=as.factor(month_tdb_created))
# 
# tdb_data.r

grph_province_ip <- ggplot(data = tdb_data3) +
  geom_bar (mapping=aes(x=province, fill=user_ips))

grph_province_ip

#grph_ip_province <- ggplot(data = tdb_data.r) +
  #geom_bar (mapping=aes(x=user_ips, fill=province)) +
  #ggtitle("TDB Submission by Province & Implementing Partner | November 2022 Submission",
          #subtitle = "Source: TDB Submission Portal")

 ggplot(data = tdb_data3) +
  geom_bar (mapping=aes(x=province, fill=user_ips)) +
  ggtitle("TDB Submission by Province & Implementing Partner | January 2023 Submission") +
   labs(x="Province", y="Number of TDBs", caption = "Data Source: TDB Submission Portal")

ggplot(data = tdb_data3) +
  geom_bar (mapping=aes(x=user_ips, fill=Month)) +
  ggtitle("TDB Submission by Implementing Partner & Creation Month | January 2023 Submission",
          subtitle = "Source: TDB Submission Portal") +
  lab=ylab("Number of TDBs") +
  xlab("Implementing Partner")

grph_province_ip

grph_ip_creation_month

old_tdb_r <- tdb_data.r %>%
  filter(most_recent_interaction_date < "2022-11-21") %>%
  mutate(month_most_recent_interact_date=month(most_recent_interaction_date)) %>%
  mutate(month_most_recent_interact_date=as.factor(month_most_recent_interact_date)) 

old_tdb_r <- old_tdb_r %>%
  filter(most_recent_interaction_date > "1900-01-02")
  

old_tdb_r$month_most_recent_interact_date

glimpse(old_tdb_r)

#fct_recode(month_tdb_created, Jan = "1", Sep = "9", Oct = "10", Nov = "11", Dec = "12")

ggplot(data = old_tdb_r) +
  geom_bar (mapping=aes(x=user_ips, fill=month_most_recent_interact_date)) +
  ggtitle("Old TDBs by IP and Month of Last Interaction | January 2023 Submission", 
          subtitle = "Source: TDB Submission Portal") +
  ylab("Number of Old TDBs") +
  xlab("Implementing Partner") 

ggplot(data = old_tdb_r) +
  geom_bar (mapping=aes(x=user_ips, fill=facility_name)) +
  ggtitle("Old TDBs by IP and Facility | November 2022 Submission", 
          subtitle = "Source: TDB Submission Portal") +
  ylab("Number of Old TDBs") +
  xlab("Implementing Partner") 


old_tdb_r$most_recent_interaction_date
