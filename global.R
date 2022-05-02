###########################################

# LOADING LIBRARIES

library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(leaflet)
library(plotly)
# library(ggplotly)
library(tidyverse)
library(scales)
library(DT)
options(scipen=999)


###########################################
###########################################

# LOADING DATA 

rm(list=ls())
dataset <- read.csv("data/databasez.csv")
census <- read.csv("data/population_long.csv")
lat_long <- read.csv("data/us_lat_long.csv")
lat_long$description <- NULL
overall_minimum_year = 1980
overall_maximum_year = 2014
all_years = c(overall_minimum_year:overall_maximum_year)

###########################################
###########################################

# FUNCTIONS

vectorBulletList <- function(vector) {
  if(length(vector > 1)) {
    paste0("<ul><li>", 
           paste0(
             paste0(vector, collpase = ""), collapse = "</li><li>"),
           "</li></ul>")   
  }
}

###########################################
###########################################

# PROCESSING

###########################################

# MAP COUNTS

total_homicide_count <- dataset %>%
  summarise(Count=n())

total_homicide_count <- sum(total_homicide_count$Count)

total_homicide_year_count <- dataset %>%
  group_by(Year) %>%
  summarise(Count=n())

main_page_summary <- dataset %>%
  group_by(Year, State) %>%
  summarise(Count=n())

main_page_summary <- merge(main_page_summary, census, by.x = c("State", "Year"), by.y = c("State", "Year"))
main_page_summary$Homicides_Per_100K <- main_page_summary$Count / (main_page_summary$Population / 100000)

main_page_summary <- merge(main_page_summary, lat_long, by.x = "State", by.y="name", all.x = TRUE)
main_page_summary$name <- NULL

missing_states <- setdiff(lat_long$name, main_page_summary$State)

puerto = cbind(rep(missing_states[1], 35),
                   all_years,
                   rep(0, 35),
                   rep(0, 35),
                   rep(0, 35),
                   rep(lat_long[lat_long$name == missing_states[1], 'latitude'], 35),
                   rep(lat_long[lat_long$name == missing_states[1], 'longitude'], 35))

rhode = cbind(rep(missing_states[2], 35),
               all_years,
               rep(0, 35),
               rep(0, 35),
               rep(0, 35),
               rep(lat_long[lat_long$name == missing_states[2], 'latitude'], 35),
               rep(lat_long[lat_long$name == missing_states[2], 'longitude'], 35))

missing_states <- data.frame(rbind(puerto, rhode))
colnames(missing_states) <- colnames(main_page_summary)

main_page_summary <- rbind(main_page_summary, missing_states)

main_page_summary$Year <- as.numeric(main_page_summary$Year)
main_page_summary$Count <- as.numeric(main_page_summary$Count)
main_page_summary$latitude <- as.numeric(main_page_summary$latitude)
main_page_summary$longitude <- as.numeric(main_page_summary$longitude)
main_page_summary$Population <- as.numeric(main_page_summary$Population)
main_page_summary$Homicides_Per_100K <- round(as.numeric(main_page_summary$Homicides_Per_100K),1)

main_page_summary <- main_page_summary %>%
  group_by(State, latitude, longitude) %>%
  complete(Year = all_years)

main_page_summary[is.na(main_page_summary)] <- 0


main_page_summary <- main_page_summary %>% arrange(State, Year)

# 
# 
# state_avg_per_year <- main_page_summary %>%
#   group_by(State) %>%
#   summarise(Avg_Per_Year_Per_100k = round(mean(Homicides_Per_100K),2))
# 
# state_avg_per_year[state_avg_per_year$State == "Rhodes Island", ][1] <- "Rhode Island" 
# state_avg_per_year <- state_avg_per_year %>% arrange(State)
# 
# state_avg_per_year <- merge(state_avg_per_year, lat_long, by.x = "State", by.y="name", all.x=TRUE)
# state_avg_per_year$name <- NULL
# state_avg_per_year <- rbind(state_avg_per_year, c("Puerto Rico", 0, 18.46633000, -66.10572000))
# state_avg_per_year <- rbind(state_avg_per_year, c("Rhode Island", 0, 41.825226, -71.418884))
# state_avg_per_year[state_avg_per_year$State == "District of Columbia", 3] <- 38.942142
# state_avg_per_year[state_avg_per_year$State == "District of Columbia", 4] <- -77.025955
# 
# main_page_summary <- merge(main_page_summary, state_avg_per_year, on = 'State', all.X = TRUE)
 
# main_page_summary <- main_page_summary %>% arrange(State, Year)



###########################################

# REGION PLOT

region_summary <- dataset %>%
  group_by(Year, State, City, Weapon) %>%
  summarise(Count = n())

temp <- region_summary %>%
  group_by(Year, State, City) %>%
  mutate(Count = sum(Count))
temp$Weapon <- "All Weapons"
region_summary <- region_summary %>%
  bind_rows(unique(temp)) %>%
  arrange(Year, State, City, Weapon)

temp <- region_summary %>%
  group_by(Year, State, Weapon) %>%
  mutate(Count = sum(Count))
temp$City <- "All Cities"
region_summary <- region_summary %>%
  bind_rows(unique(temp)) %>%
  arrange(Year, State, City, Weapon)

temp <- region_summary %>%
  group_by(Year, City, Weapon) %>%
  mutate(Count = sum(Count))
temp$State <- "All States"
region_summary <- region_summary %>%
  bind_rows(unique(temp)) %>%
  arrange(Year, State, City, Weapon)

region_summary$Year <- as.numeric(region_summary$Year)
region_summary <- distinct(region_summary)

state_list <- unique(region_summary$State)
city_list <- unique(region_summary$City)
weapon_list <- unique(region_summary$Weapon)

###########################################

# COMPARISON PLOT

comparison_age <- dataset %>%
  group_by(Victim.Age) %>%
  summarise(Count = n())

comparison_age$Victim.Age <- as.factor(comparison_age$Victim.Age)

comparison_race <- dataset %>%
  group_by(Victim.Race) %>%
  summarise(Count = n())

comparison_race$Victim.Race <- as.factor(comparison_race$Victim.Race)

comparison_sex <- dataset %>%
  group_by(Victim.Sex) %>%
  summarise(Count = n())

comparison_sex$Victim.Sex <- as.factor(comparison_sex$Victim.Sex)

colnames(comparison_age)[1] <- "Metric"
colnames(comparison_race)[1] <- "Metric"
colnames(comparison_sex)[1] <- "Metric"

###########################################
###########################################

# STRINGS

intro_text <- "This app is a look into the history of homicides in the United States.\
Each tab looks into some of the key factors relating to homicides between 1980 and 2014,\
both inclusive."

tab_text <- "The USA Homicide Incidents tab gives a Map+Bubble visualisation of homicides over the years.\
The Region Plots tab shows Homicides by Region and the Comparison tab a comparison based on factors \
like Age, Gender, and Race of the victims"

dataset_info <- "The dataset in use is the homicide data provided with 638454 rows and 24 columns \
Each row provides information for a single case of homicide. The 24 columns are given below:"

summary_text <- "Above, we can see how homicides have declined over the years in question.\
Below, we have a preview of the summarised dataset we will be using as reference further. The data \
is arranged alphabetically by States followed by ascending order of Year."

original_columns <- vectorBulletList(colnames(dataset))

other_data_text <- 'Additionally, there was also need to source latitude and longitude data and compiling\
it manually. Along with that, the US Population Data was also sourced from'

###########################################
###########################################

