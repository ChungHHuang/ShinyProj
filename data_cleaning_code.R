library(data.table)
library(tidyverse)
library(lubridate)
library(googleVis)
library(leaflet)

# Read csv files and combine them to a single dataframe
files <- list.files(path = "~/Project_data/",pattern = ".csv")
temp <- lapply(files, fread, sep=",")
nride <- rbindlist( temp )

# Replace space with _ in the column name
names(nride) = gsub(' ','_',names(nride),fixed = T)

# Convert start and end time to POSIXct type
nride$start_time = as.POSIXct(nride$start_time)
nride$end_time = as.POSIXct(nride$end_time)

# Obtain month, hours, and weekdays from start time
nride$month = month(nride$start_time)
nride$hour = hour(nride$start_time)
nride$weekday = weekdays(nride$start_time, abbr = TRUE)

# Reorder weekdays
nride$weekday = ordered(nride$weekday, levels=c("Mon", "Tue", "Wed", "Thu", 
                                         "Fri", "Sat", "Sun"))

# Make a new column showing weekday and weekend
nride$weekday_weekend = ifelse(nride$weekday %in% c('Sat','Sun'),"weekend",'weekday')

# Transfer gender to char
nride$gender = ifelse(nride$gender==1,"Male",ifelse(nride$gender==2,"Female","Unknown"))

# Get user age from birth year
nride$age = year(now()) - nride$birth_year

# Delete these columns
nride$birth_year = NULL
nride$bikeid = NULL
nride$start_station_id = NULL
nride$end_station_id = NULL

# Rename columns
nride = nride %>% rename(t_duration = tripduration)
nride = nride %>% rename(s_s_name = start_station_name)
nride = nride %>% rename(s_s_lat = start_station_latitude)
nride = nride %>% rename(s_s_long = start_station_longitude)
nride = nride %>% rename(e_s_name = end_station_name)
nride = nride %>% rename(e_s_lat = end_station_latitude)
nride = nride %>% rename(e_s_long = end_station_longitude)
nride$s_s_name = trimws(nride$s_s_name)

# A station was moved, so make the lat and long the same
nride$s_s_lat[nride$s_s_lat==44.9765154821856] = 44.9762199636466
nride$e_s_lat[nride$e_s_lat==44.9765154821856] = 44.9762199636466
nride$s_s_long[nride$s_s_long==-93.2525700330734] = -93.2519772648811
nride$e_s_long[nride$e_s_long==-93.2525700330734] = -93.2519772648811

# Too many unknown user are borned in 1969, and so users are more than 100 years old.
# Make a new df and add a new column called age_group
nage = nride %>% filter(!(gender == "Unknown" & age == 50), age < 82) %>%
  mutate(age_group = ifelse(age <= 25, '18-25',
                            ifelse(
                              age <= 35, '26-35',
                              ifelse(age <= 45, '36-45',
                                     ifelse(
                                       age <= 55, '46-55',
                                       ifelse(age <= 65, '56-65', '65+')
                                     ))
                            )))

# Read the light rail stations data and only retain three columns
l_r_station = read.csv("light_rail_stations.csv")
l_r_station = l_r_station %>% select(stop_name,stop_lat,stop_lon)

################################################
# Write cleaned data to csv files
write.csv(nride, file='data.csv', row.names=F)
write.csv(nage, file='age_data.csv',row.names = F)
write.csv(l_r_station, file='light_rail_stations.csv', row.names=F)
################################################

# Test code####
# Since it is inefficient to debug in Shiny, the codes below are the drafts before they are put into Shiny app
# 
# # Who are they in gender?
# nride %>% ggplot(aes(gender)) + geom_bar(aes(fill = usertype),position = 'dodge')
# gvisColumnChart(nride,xvar = "age")
# # How old are them?
# nage %>% ggplot(aes(age)) + geom_bar(aes(fill = gender))
# nage %>% ggplot(aes(age)) + geom_density(aes(fill = gender),alpha = 0.5)
# nage %>% ggplot(aes(age)) + geom_bar(aes(fill = usertype),position = "fill")
# nage %>% ggplot(aes(age)) + geom_bar(aes(fill = bike_type))
# nage %>% ggplot(aes(age)) + geom_bar(aes(fill = gender),position = "fill")
# nage %>% ggplot(aes(gender,age)) + geom_boxplot()
# nage %>% ggplot(aes(age_group)) + geom_bar(aes(fill = isde),position = "fill")
# # Customer vs Subscriber
# nage %>% ggplot(aes(usertype,age)) + geom_boxplot()  
# nride %>% ggplot(aes(usertype)) + geom_bar(aes(fill=gender))
# # Bike type
# nride %>% ggplot(aes(bike_type)) + geom_bar(aes(fill=usertype))
# nride %>% ggplot(aes(bike_type)) + geom_bar(aes(fill=gender))
# 
# # When?
# # Weekday
# nride %>% ggplot(aes(weekday)) + geom_bar(aes(fill=gender),position = "fill")
# nride %>% ggplot(aes(weekday)) + geom_bar(aes(fill=usertype),position = "fill")
# nride %>% ggplot(aes(weekday)) + geom_bar(aes(fill=bike_type))
# nage %>% ggplot(aes(weekday,age)) + geom_boxplot()
# nage %>% filter(weekday=="Sat") %>% ggplot(aes(age)) + geom_bar(aes(fill=gender))
# # Hours
# nride %>% ggplot(aes(hour)) + geom_bar(aes(fill=isde))
# nride %>% filter(weekday=="Fri") %>% ggplot(aes(hour)) + geom_bar(aes(fill=gender))
# nage %>% ggplot(aes(hour)) + geom_bar(aes(fill=age_group),position = "fill")
# # Month
# nride %>% ggplot(aes(month)) + geom_bar(aes(fill=isde),position = "fill")
# 
# # Where?
# nride %>% filter(s_s_name!="NULL") %>% 
#   group_by(s_s_name,s_s_lat,s_s_long) %>% 
#   summarise(count = n()) %>% arrange(desc(count)) %>% top_n(10, count) 
# 
# nride %>% filter(e_s_name!="NULL") %>% 
#   group_by(e_s_name) %>% 
#   summarise(count = n()) %>% top_n(10, count)
# 
# nride %>% filter(s_s_name=="6th Ave SE & University Ave") %>% 
#   ggplot(aes(month)) + geom_bar()
# 
# nride %>%
#   mutate(Hour = hour(start_time), Year = year(start_time), Month = month(start_time)) %>%
#   group_by(Year, Month, Hour, gender) %>% summarise(n=n()) %>% ggplot(aes(Hour,n)) + 
#   geom_col(aes(fill=gender))
# 
# ############################
# #     Map
# ############################
# 
# # Find the most popular stations
# tops = head(nride %>% filter(s_s_name!="NULL") %>% 
#   group_by(s_s_name,s_s_lat,s_s_long) %>% 
#   summarise(count = n()) %>% arrange(desc(count)),10)
# top10 = head(nride %>% filter(s_s_name %in% tops$s_s_name) %>% 
#   transmute(s_s_name,s_s_long,s_s_lat) %>% distinct(),15)
# 
# test1 = nride %>% filter(s_s_name!="NULL") %>% 
#   group_by(s_s_name) %>% 
#   summarise(count = n())
# 
# top15 = unique(left_join(nride,test1,by="s_s_name") %>% 
#   transmute(s_s_name,s_s_long,s_s_lat,count)) %>% arrange(desc(count))%>% top_n(20,count)
# 
# 
# nride %>% group_by(s_s_name)
# leaflet(l_r_station) %>% addTiles() %>% 
#   addMarkers(lng = ~stop_lon,lat = ~stop_lat, popup = ~stop_name)
# leaflet(top10) %>% addTiles() %>% 
#   addMarkers(lng = ~s_s_long,lat = ~s_s_lat, popup = ~s_s_name)
# 
# nride %>% filter(e_s_name=="Lake Street & Knox Ave S") %>% 
#   ggplot(aes(month)) + geom_bar(aes(fill=usertype))
# 
# ###########################
# # Find the closest station
# start_lat = nride[nride$s_s_name=="McNamara Center",][[1,6]]
# start_long= nride[nride$s_s_name=="McNamara Center",][[1,7]]
# s_temp = (nride %>% filter(s_s_name=="McNamara Center",e_s_name=="McNamara Center"))[1,]
# start_lat = s_temp[[6]]
# l_r_station 
# min_d =1000
# k =0
# for (i in 1:nrow(l_r_station)) {
#   if ((l_r_station[[i,2]]-start_lat)**2+(l_r_station[[i,3]]-start_long)**2 < min_d){
#     min_d = (l_r_station[[i,2]]-start_lat)**2+(l_r_station[[i,3]]-start_long)**2 
#     k = i
#   }
# }
# l_r_station[k,]
# min_d2 = 1000
# l = 0
# e_temp = nride %>% filter(s_s_name=="McNamara Center") %>% group_by(e_s_name,e_s_lat,e_s_long) %>% summarise(count = n())
# for (i in 1:nrow(e_temp)) {
#   if ((l_r_station[[k,2]]-e_temp[[i,2]])**2+(l_r_station[[k,3]]-e_temp[[i,3]])**2 < min_d2){
#     min_d2 = (l_r_station[[k,2]]-e_temp[[i,2]])**2+(l_r_station[[k,3]]-e_temp[[i,3]])**2
#     l = e_temp[[i,1]]
#   }
# }
# l
# 
# (nride %>% filter(s_s_name=="McNamara Center",e_s_name==l) %>% summarise(median(t_duration)))[[1]]


















