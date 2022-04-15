### Cyclistic_Full_Year_Analysis ###


# This analysis is based on  the Cyclistic bike-share case study!  
# The steps of the data analysis process: ask, prepare, process, analyze, share, and act.
# Data used in the case study is: (found here: https://divvy-tripdata.s3.amazonaws.com/index.html )
# The purpose of this script is to consolidate downloaded data into a single data frame
# Then conduct simple analysis to help answer the key question: 
# In what ways do members and casual riders use Cyclistic bikes differently?


# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# lubridate for date functions
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  


library(tidyverse)  
library(lubridate)  
library(ggplot2)  
getwd() 
setwd("C:/Users/sumit kumar/Desktop/Cyclistic") 


#=====================
# STEP 1: COLLECT DATA
#=====================


# Upload Cyclistic data sets (csv files) here

q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")


#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================


# Compare column names of each of the files

colnames(q3_2019)
colnames(q4_2019)
colnames(q2_2019)
colnames(q1_2020)

# Rename columns  to make them consistent with q1_2020 

(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))


(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))


(q2_2019 <- rename(q2_2019  
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))

# Inspect the data frames and look for incongruencies

str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

# Convert ride_id and rideable_type to character so that they can stack correctly

q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

# Stack individual quarter's data frames into one big data frame

all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

# Remove lat, long, birth year, and gender fields as this data was dropped beginning in 2020

all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))


#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================


# Inspect the new table that has been created

colnames(all_trips)  
nrow(all_trips)  
dim(all_trips)  
head(all_trips) 
str(all_trips)  
summary(all_trips)  


# There are a few problems needed to be fixed:
# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). Let's consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. Let's add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) Let's add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. Now add "ride_length" to the entire data frame for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Cyclistic took bikes out of circulation for Quality Control reasons. Let's delete these rides.

# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Cyclistic used different labels for these two types of riders ... Let's make our data frame consistent with their current nomenclature
# Begin by seeing how many observations fall under each usertype

table(all_trips$member_casual)

# Reassign to the desired values (let's go with the current 2020 labels)

all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Check to make sure the proper number of observations were reassigned

table(all_trips$member_casual)

# Add columns that list the time, date, month, day, and year of each ride

all_trips$date <- as.Date(all_trips$started_at) 
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$time <- format(as.POSIXct(all_trips$started_at), format = "%H:%M")
all_trips$time <- as.POSIXct(all_trips$time, format = "%H:%M")

# Add a "ride_length" calculation to all_trips (in seconds)

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns

str(all_trips)

# Convert "ride_length" from Factor to numeric so calculations can be run on the data

is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The data frame includes a few hundred entries when bikes were taken out of docks and checked for quality by Cyclistic or ride_length was negative
# Let's create a new version of the data frame (v2) since data is being removed

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]


#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================


# Descriptive analysis on ride_length (all figures in seconds)

mean(all_trips_v2$ride_length) 
median(all_trips_v2$ride_length) 
max(all_trips_v2$ride_length) 
min(all_trips_v2$ride_length) 

summary(all_trips_v2$ride_length)

# Compare members and casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# The days of the week are out of order. Let's fix that.

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()							
            ,average_duration = mean(ride_length)) %>% 		
  arrange(member_casual, weekday)								

# Let's visualize the number of rides by rider type

all_trips_v2 %>%  
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n() ) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + 
  labs(x= 'Day of Week', y='Total Number of Rides', title='Rides per Day of Week', fill = 'Type of Membership') +
  scale_y_continuous(breaks = c(100000, 250000, 400000, 550000), labels = c("100K", "250K", "400K", "550K"))

# Let's create a visualization for average duration

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +geom_col(position = "dodge")+
  labs(x= 'Day of Week', y='Average Ride Length(sec)', title='Average Ride Length by Customer Type and Day of Week', fill = 'Type of Membership')

# total rides broken down by month

all_trips_v2 %>%   
  group_by(member_casual, month) %>%  
  summarise(number_of_rides = n(),`average_duration_(secs)` = mean(ride_length)) %>% 
  arrange(member_casual) %>% 
  ggplot(aes(x=month, y=number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + 
  labs(x= "Month", y= "Total Number of Rides", title = "Rides per Month", fill = "Type of Membership") + 
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000), labels = c("100K", "200K", "300K", "400K")) + theme(axis.text.x = element_text(angle = 45))

# Average ride length by customer type and month

all_trips_v2 %>%  
  group_by(member_casual, month) %>% 
  summarise(average_ride_length = mean(ride_length)) %>% 
  ggplot(aes(x=month, y = average_ride_length, fill = member_casual))+ 
  geom_col(position = "dodge") +
  labs (x="Month", y = "Average Ride Length(sec)", title = "Average Ride Length by Customer Type and Month", 
        fill = "Type of Membership") + theme(axis.text.x = element_text(angle = 45))

# Looking at demand over a 24 hour day

all_trips_v2 %>%     
  group_by(member_casual, time) %>% 
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(x=time, y=number_of_rides, color = member_casual )) +
  geom_line() + scale_x_datetime(date_breaks = "1 hour",
                                 date_labels = "%H:%M", expand = c(0,0)) +
  labs(title ="Demand Throughout the Day", x = "Time", y = "Total Number of Rides",color = "Type of Membership")+
  theme(axis.text.x = element_text(angle = 45)) 
  


#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================


# Creating a csv file that will be visualized in Tableau.

write.csv(all_trips_v2, file = 'C:/Users/sumit kumar/Desktop/Cyclistic/all_trips.csv')

#  !!!! DONE !!!!