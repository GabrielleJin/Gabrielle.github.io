install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
library("tidyverse")
library("lubridate")
library("ggplot2")

getwd()
setwd("/Users/dj/Desktop/Case Study/CSV File")

July_20 <- read_csv("202007-divvy-tripdata.csv")
Aug_20 <- read_csv("202008-divvy-tripdata.csv")
Sep_20 <- read_csv("202009-divvy-tripdata.csv")
Oct_20 <- read_csv("202010-divvy-tripdata.csv")
Nov_20 <- read_csv("202011-divvy-tripdata.csv")
Dec_20 <- read_csv("202012-divvy-tripdata.csv")
Jan_21 <- read_csv("202101-divvy-tripdata.csv")
Feb_21 <- read_csv("202102-divvy-tripdata.csv")
March_21 <- read_csv("202103-divvy-tripdata.csv")
April_21 <- read_csv("202104-divvy-tripdata.csv")
May_21 <- read_csv("202105-divvy-tripdata.csv")
June_21 <- read_csv("202106-divvy-tripdata.csv")

colnames(July_20)
colnames(Aug_20)
colnames(Sep_20)
colnames(Oct_20)
colnames(Nov_20)
colnames(Dec_20)
colnames(Jan_21)
colnames(Feb_21)
colnames(March_21)
colnames(April_21)
colnames(May_21)
colnames(June_21)

str(July_20)
str(Aug_20) 
str(Sep_20)
str(Oct_20)
str(Nov_20)
str(Dec_20)
str(Jan_21)
str(Feb_21)
str(March_21)
str(April_21)
str(May_21)
str(June_21)

#Convert date and time to the same format
July_20 <- mutate(July_20, started_at = mdy_hm(started_at), ended_at = mdy_hm(ended_at))
Oct_20 <- mutate(Oct_20, started_at = mdy_hm(started_at), ended_at = mdy_hm(ended_at))
Jan_21 <- mutate(Jan_21, started_at = mdy_hm(started_at), ended_at = mdy_hm(ended_at))
Feb_21 <- mutate(Feb_21, started_at = mdy_hm(started_at), ended_at = mdy_hm(ended_at))
March_21 <- mutate(March_21, started_at = mdy_hm(started_at), ended_at = mdy_hm(ended_at))
June_21 <- mutate(June_21, started_at = mdy_hm(started_at), ended_at = mdy_hm(ended_at))

#Convert start_station_id and end_station_id to character
July_20 <- mutate(July_20, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
Aug_20 <- mutate(Aug_20, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
Sep_20 <- mutate(Sep_20, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
Oct_20 <- mutate(Oct_20, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
Nov_20 <- mutate(Nov_20, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
Dec_20 <- mutate(Dec_20, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

#Stack individual monthly's data frames into one big data frame
All_trips <- bind_rows(July_20,Aug_20,Sep_20,Oct_20,Nov_20,Dec_20,Jan_21,Feb_21,March_21,April_21,May_21,June_21)

All_trips <- All_trips %>% select(-c(start_lat,start_lng,end_lat,end_lng,start_station_id,end_station_id,end_station_name)) 

#Inspect the new table that has been created
colnames(All_trips)
nrow(All_trips)
dim(All_trips)
head(All_trips)
str(All_trips)
summary(All_trips)

#check how many types in member_casual
unique(All_trips[c("member_casual")])
table(All_trips$member_casual)

#Add new columns in the table
All_trips$month <- format(All_trips$started_at,"%m")
All_trips$date <- format(All_trips$started_at,"%d")      
All_trips$year <- format(All_trips$started_at,"%y")
All_trips$day_of_week <- format(All_trips$started_at,"%A")
All_trips$ride_length <- difftime(All_trips$ended_at,All_trips$started_at)

typeof(All_trips$ride_length)
All_trips$ride_length <- as.numeric(All_trips$ride_length)
is.numeric(All_trips$ride_length)

#Remove ride_length 
All_trips_v2 <- filter(All_trips, ride_length > 0)

#Conduct descriptive analysis

#Descriptive analysis on ride_length (all figures in seconds)
summary(All_trips_v2$ride_length)

#Compare members and casual users
aggregate(All_trips_v2$ride_length ~ All_trips_v2$member_casual, FUN = mean)
aggregate(All_trips_v2$ride_length ~ All_trips_v2$member_casual, FUN = median)
aggregate(All_trips_v2$ride_length ~ All_trips_v2$member_casual, FUN = max)
aggregate(All_trips_v2$ride_length ~ All_trips_v2$member_casual, FUN = min)

#See the average ride time by each day for members vs casual users
aggregate(All_trips_v2$ride_length ~ All_trips_v2$member_casual + All_trips_v2$day_of_week, FUN = mean)

#Fix the order of the days of the week and re-run
All_trips_v2$day_of_week <- ordered(All_trips_v2$day_of_week, level=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
aggregate(All_trips_v2$ride_length ~ All_trips_v2$member_casual + All_trips_v2$day_of_week, FUN = mean)

#Analyze ridership data by type and weekday
All_trips_v2 %>% 
  group_by(member_casual,day_of_week) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual,day_of_week)

#Visualize the number of rides by rider type

All_trips_v2 %>% 
  group_by(member_casual,day_of_week) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual,day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") 

#Export Summary File for Future Analysis
counts <- aggregate(All_trips_v2$ride_length ~ All_trips_v2$member_casual + All_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = "/Users/dj/Desktop/Case Study/avg_ride_length.csv")