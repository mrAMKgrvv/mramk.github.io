install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("geosphere")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(geosphere)
#Importing the csv files #mine kept saving as csv.csv
January23 <-read_csv("cyclistic/January 23.csv.csv")
February23 <-read_csv("cyclistic/February 23.csv.csv")
March23 <-read_csv("cyclistic/March 23.csv.csv")
April23 <-read_csv("cyclistic/April 23.csv.csv")
May23 <-read_csv("cyclistic/May 23.csv.csv")
June23 <-read_csv("cyclistic/June 23.csv.csv")
July23 <-read_csv("cyclistic/July 23.csv.csv")
August23 <-read_csv("cyclistic/August 23.csv.csv")
September23 <-read_csv("cyclistic/September 23.csv.csv")
October23 <-read_csv("cyclistic/October 23.csv.csv")
November23 <-read_csv("cyclistic/November 23.csv.csv")
December23 <-read_csv("cyclistic/December 23.csv.csv")

#Binding Rows
trip_data <- bind_rows(February23, March23, April23, May23, June23, July23, August23, September23, October23, November23, December23, January23)

head(trip_data) #see the first 6 rows of the data frame
nrow(trip_data) #how many rows are in the data frame
colnames(trip_data) #list of column names
dim(trip_data) #dimensions of the data frame
summary(trip_data) #statistical summary of data, mainly for numerics
str(trip_data) #see list of columns and data types
tail(trip_data) #see the last 6 rows of the data frame

#Adding columns for date, month, year, day of the week into the data frame.
trip_data$date <- as.Date(trip_data$started_at) 
trip_data$month <- format(as.Date(trip_data$date), "%m")
trip_data$day <- format(as.Date(trip_data$date), "%d")
trip_data$year <- format(as.Date(trip_data$date), "%Y")
trip_data$day_of_week <- format(as.Date(trip_data$date), "%A")
colnames(trip_data) #to get the names of all the columns

#Add a ride_length calculation to trip_data
trip_data$ride_length <- difftime(trip_data$ended_at, trip_data$started_at)
str(trip_data) #to inspect the structure of the columns

#Convert ride_length from Factor to Numeric in order to run calculations
trip_data$ride_length <- as.numeric(as.character(trip_data$ride_length))
is.numeric(trip_data$ride_length) #to confirm it is now numeric

#Add ride_distance calculation to trip_data
trip_data$ride_distance <- distGeo(matrix(c(trip_data$start_lng, trip_data$start_lat), ncol=2), matrix (c(trip_data$end_lng, trip_data$end_lat), ncol=2))
trip_data$ride_distance <- trip_data$ride_distance/1000 #distance in km

#Remove "bad" data
#The data frame includes a few hundred entries when bikes where taken out of docks and checked for quality by Divvy where ride_length was negative or "zero"
trip_data_clean <- trip_data[!(trip_data$ride_length <= 0),]
glimpse(trip_data_clean) #gives column names and data in the column

str(trip_data_clean) #first lets check the structure of the data frame
summary(trip_data_clean) #to check the summarized details of the clean data frame

##Filtering and separating the table into Members and Casual

Member <-filter(trip_data_clean, trip_data_clean$member_casual=='member')
Casual <-filter(trip_data_clean, trip_data_clean$member_casual=='casual')
glimpse(Member)
glimpse(Casual)

# Calculate the average and Total Duration for Casual and Member.

x <- Member$ride_length
sum(x)
mean(x) 
median(x)
max(x)
min(x)

y <-Casual$ride_length
sum(y)  
mean(y)
median(y)
max(y) 
min(y) 

#lets order the days of the week
trip_data_clean$day_of_week <- ordered(trip_data_clean$day_of_week, 
                                       levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
trip_data_clean %>% 
  group_by(member_casual, day_of_week) %>%  #groups by member_casual
  summarise(number_of_rides = n() #calculates the number of rides and average duration 
            ,average_ride_length = mean(ride_length),.groups="drop") %>% # calculates the average duration
  arrange(member_casual, day_of_week) #sort

#let's visualize the above table by days of the week and number of rides taken by member and casual riders.
trip_data_clean %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups="drop") %>% 
  arrange(member_casual, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides of Members and Casual riders Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#Let's visualize average ride by day of the week
trip_data_clean %>%  
  group_by(member_casual, day_of_week) %>% 
  summarise(average_ride_length = mean(ride_length), .groups="drop") %>%
  ggplot(aes(x = day_of_week, y = average_ride_length, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average ride time of Members and Casual riders Vs. Day of the week")

#Let's visualize the total rides taken by members and casuals by month
trip_data_clean %>%  
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(),.groups="drop") %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total rides by Members and Casual riders by Month") +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#Let's compare Members and Casual riders depending on ride distance
trip_data_clean %>% 
  group_by(member_casual) %>% drop_na() %>%
  summarise(average_ride_distance = mean(ride_distance)) %>%
  ggplot() + 
  geom_col(mapping= aes(x= member_casual,y= average_ride_distance,fill=member_casual), show.legend = FALSE)+
  labs(title = "Mean distance traveled by Members and Casual riders")

write.csv(trip_data_clean, "capstone_data.csv")
