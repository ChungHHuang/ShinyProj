## global.R ##
library(shiny)
library(data.table)
library(tidyverse)
library(googleVis)
library(leaflet)
library(DT)
# Main df
nride = fread(file = "data.csv")
# Light rail station df
l_r_station = fread(file = "light_rail_stations.csv")
# Df for anything relate to age
nage = fread(file = "age_data.csv")
# Reroder weekday 
nride$weekday = ordered(nride$weekday, levels=c("Mon", "Tue", "Wed", "Thu","Fri", "Sat", "Sun"))
nage$weekday= ordered(nage$weekday, levels=c("Mon", "Tue", "Wed", "Thu","Fri", "Sat", "Sun"))

# Choice vectors
user_choice = c("gender","age","age_group","bike_type","usertype")
user_choice2 = c("gender","age_group","bike_type","usertype")
time_choice = c("weekday",'hour','month')
time_choice2= c("gender","age_group","bike_type","usertype",'weekday_weekend')
station_choice2 = c("gender","age_group","usertype",'weekday_weekend')
style = c("dodge","fill",'stack')
stat_choice = (nride %>% group_by(s_s_name) %>% summarise(n = n()) %>% arrange(desc(n)))[-1,]$s_s_name

# Plot theme
theme_update(plot.title = element_text(hjust = 0.5),
             axis.text=element_text(size=12),
             axis.title=element_text(size=14,face="bold"),
             legend.text=element_text(size=12),
             legend.title=element_text(size=12),
             legend.title.align=0.5)