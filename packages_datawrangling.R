


library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(shinythemes)
library(rsconnect)
library(reshape2)
library(plotrix)
library(lubridate)
library(shinydashboard)
library(zoo)
library(stringr)
options(scipen = 999)


d <- read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"))




dfall<- d%>%
  group_by(location,iso_code, total_vaccinations,people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred , date)%>%
  mutate(location = recode(location, 	"Israel" = "Palestinian occupied territories"))%>%
  summarize( Lateset_update = max(date))%>%
  arrange(desc(total_vaccinations))%>%
  filter(!(str_detect(iso_code, "OWID_")))%>%
  select(location , total_vaccinations ,people_fully_vaccinated_per_hundred, people_vaccinated_per_hundred, Lateset_update)
dfallworld <- distinct(dfall, location, .keep_all = TRUE)

dfall_DT<- d%>%
  group_by(location,iso_code, daily_vaccinations,total_vaccinations, people_fully_vaccinated_per_hundred , date,people_vaccinated_per_hundred)%>%
  arrange(desc(total_vaccinations))%>%
  mutate(location = recode(location, 	"Israel" = "Palestinian occupied territories"))%>%
  filter(!(str_detect(iso_code, "OWID_")))%>%
  select(location , date , daily_vaccinations,total_vaccinations,people_fully_vaccinated_per_hundred, people_vaccinated_per_hundred)
dfallworld_DT <- distinct(dfall_DT, location, .keep_all = TRUE)


df_top <- d%>%
  group_by(location,iso_code, daily_vaccinations, date )%>%
  summarize(total_vaccinated_people = max(total_vaccinations))%>%
  mutate(total_vaccinated_people = as.numeric( total_vaccinated_people))%>%
  arrange(desc( total_vaccinated_people))%>%
  mutate(location = recode(location, 	"Israel" = "Palestinian occupied territories"))%>%
  filter(!(str_detect(iso_code, "OWID_")))
df1 <- distinct(df_top, total_vaccinated_people, .keep_all = TRUE)

df_top_saudi <- d%>%
  group_by(location,iso_code,people_vaccinated, daily_vaccinations, date )%>%
  summarize(total_vaccinated_people = max(people_vaccinated))%>%
  mutate(total_vaccinated_people = as.numeric( total_vaccinated_people))%>%
  arrange(desc( total_vaccinated_people))%>%
  mutate(location = recode(location, 	"Israel" = "Palestinian occupied territories"))%>%
  filter(!(str_detect(iso_code, "OWID_")))%>%
  filter(location == "Saudi Arabia")
df_saudi_1 <- distinct(df_top_saudi, total_vaccinated_people, .keep_all = TRUE)

df_top_saudi_2 <- d%>%
  group_by(location,iso_code,  people_fully_vaccinated, date )%>%
  summarize(Fully_total_vaccinated_people = max(people_fully_vaccinated))%>%
  mutate(Fully_total_vaccinated_people = as.numeric( Fully_total_vaccinated_people))%>%
  arrange(desc( Fully_total_vaccinated_people))%>%
  mutate(location = recode(location, 	"Israel" = "Palestinian occupied territories"))%>%
  filter(!(str_detect(iso_code, "OWID_")))%>%
  filter(location == "Saudi Arabia")
df_saudi_2 <- distinct(df_top_saudi_2, Fully_total_vaccinated_people, .keep_all = TRUE)



piechart_1 <- d %>%    
  group_by(location, people_vaccinated_per_hundred, date)%>%
  summarize( lateset_update = max(date), total_vaccinated_people = max(people_vaccinated_per_hundred))%>%
  arrange(desc(total_vaccinated_people))%>%
  filter(location == "World")%>%
  mutate( Curren_no._of_population =  c(100))%>%
  mutate(Curren_no._of_population = as.numeric(Curren_no._of_population))%>%
  mutate(total_vac = as.numeric(total_vaccinated_people))%>%
  group_by( Curren_no._of_population)%>%
  top_n(1, lateset_update )%>%
  select(lateset_update, total_vac )

total_llworld <- d%>%
  group_by( location, people_fully_vaccinated, people_vaccinated, date)%>%
  summarize( Lateset_Update = max(date), Total_Vaccinated_People = max(people_vaccinated))%>%
  arrange(desc(Total_Vaccinated_People))%>%
  filter(location == "World")


info_box_total<- total_llworld%>% group_by(location)%>%
  top_n(1, Total_Vaccinated_People )%>%
  select(location, Total_Vaccinated_People)

info_box_latest_date<- total_llworld%>% group_by( location)%>%
  top_n(1, Total_Vaccinated_People )%>%
  select(date, location, Total_Vaccinated_People)
#drop location
info_box_total1 <- subset(info_box_total, select = -c(location))
#drop location + total
info_box_latest_date2 <- subset(info_box_latest_date, select = -c(location,Total_Vaccinated_People))



Boosters <- d%>%
  group_by( location,  people_fully_vaccinated, date)%>%
  summarize( Lateset_Update = max(date), totalBoost = max(people_fully_vaccinated))%>%
  arrange(desc(totalBoost))%>%
  filter(location == "World")


TotalBoosters<- Boosters%>% group_by(location)%>%
  top_n(1, totalBoost )%>%
  select(location, totalBoost)
#drop location
info_box_booster <- subset(TotalBoosters, select = -c(location))

total_l_SA <- d%>%
  group_by( location,daily_vaccinations, daily_vaccinations_per_million,people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred, people_vaccinated, date)%>%
  summarize( Lateset_Update = max(date), Total_Vaccinated_People = max(people_vaccinated_per_hundred))%>%
  arrange(desc(Total_Vaccinated_People))%>%
  filter(location == "Saudi Arabia")

info_box_total_SA<- total_l_SA%>% group_by(location)%>%
  top_n(1, people_vaccinated )%>%
  select(location, people_vaccinated)

info_box_latest_date_SA<- total_l_SA%>% group_by( location)%>%
  top_n(1, Total_Vaccinated_People )%>%
  select(date, location, Total_Vaccinated_People)
#drop location
info_box_total_SA1 <- subset(info_box_total_SA, select = -c(location))
#drop location + total
info_box_latest_date_SA2 <- subset(info_box_latest_date_SA, select = -c(location,Total_Vaccinated_People))


Boosters_SA <- d%>%
  group_by( location,  people_fully_vaccinated, date)%>%
  summarize( Lateset_Update = max(date), totalBoost = max(people_fully_vaccinated))%>%
  arrange(desc(totalBoost))%>%
  filter(location == "Saudi Arabia")


TotalBoosters_SA<- Boosters_SA%>% group_by(location)%>%
  top_n(1, totalBoost )%>%
  select(location, totalBoost)
#drop location
info_box_booster_SA <- subset(TotalBoosters_SA, select = -c(location))

#percentage fo SA
info_box_totalPercent_SA<- total_l_SA%>% group_by(location)%>%
  top_n(1, people_vaccinated_per_hundred )%>%
  select(location, people_vaccinated_per_hundred)

info_box_FullyPercent_SA<- total_l_SA%>% group_by(location)%>%
  top_n(1, people_fully_vaccinated_per_hundred )%>%
  select(location, people_fully_vaccinated_per_hundred)

info_box_DailyVax_SA<- total_l_SA%>% group_by(location)%>%
  top_n(1, daily_vaccinations )%>%
  select(location, daily_vaccinations)


info_box_totalPercent_SA1 <- subset(info_box_totalPercent_SA, select = -c(location))

info_box_FullyPercent_SA1 <- subset(info_box_FullyPercent_SA, select = -c(location))

info_box_DailyVax_SA1 <- subset(info_box_DailyVax_SA, select = -c(location))
