---
title: "divvy_bike_trip_portfolio"
author: "David Ben-Dumbari"
date: "2024-02-02"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Cyclistic bike-share analysis

*The case study follows the six step data analysis process:*

### ❓ [Ask](#1-ask)

### 💻 [Prepare](#2-prepare)

### 🛠 [Process](#3-process)

### 📊 [Analyze](#4-analyze)

### 📋 [Share](#5-share)

### 🚲 [Act](#6-act)

## Scenario

In 2016, Cyclistic launched a successful bike-share offering. The company's future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members.

## **PHASE 1:**[❓ Ask]

💡 **BUSINESS TASK: Analyze Divvy's riding data to develop digital marketing strategies to convert casual riders into annual members.**

Primary stakeholders: The director of marketing Lily Moreno and Cyclistic executive team.

Secondary stakeholders: Cyclistic marketing analytics team. Three questions will guide the future marketing program: 1. How do annual members and casual riders use Cyclistic bikes differently? 2. why would casual riders buy Cyclistic annual memberships? 3. How can Cyclistic use digital media to influence casual riders to become members?

## **PHASE 2:** [💻 Prepare]

The data that we will be using is Cyclistic's historical trip data from last 12 months (May-2020 to Apr-2021). The data has been made available by Motivate International Inc. on this [link](https://divvy-tripdata.s3.amazonaws.com/index.html) under this [license](https://www.divvybikes.com/data-license-agreement).

The dataset consists of 12 CSV files (each for a month) with 13 columns and more than 4 million rows.

ROCCC approach is used to determine the credibility of the data

-   **R**eliable -- It is complete and accurate and it represents all bike rides taken in the city of Chicago for the selected duration of our analysis.
-   **O**riginal - The data is made available by Motivate International Inc. which operates the city of Chicago's Divvy bicycle sharing service which is powered by Lyft.
-   **C**omprehensive - the data includes all information about ride details including starting time, ending time, station name, station ID, type of membership and many more.
-   **C**urrent -- It is up-to-date as it includes data until end of May 2021
-   **C**ited - The data is cited and is available under Data License Agreement.

**Data Limitation**

A quick filtering and checking data for completeness shows that "start station name and ID" and "end station name and ID" for some rides are missing. Further observations suggest that the most missing data about "start station name" belongs to "electric bikes" as 201,975 out of 888,490 electric ride shares have missing data and it accounts for 22% of total electric-bike ride shares.

This limitation could slightly affect our analysis for finding stations where most electric-bikes are taken but we can just observe our available data for now. \<[endif]--\>

## **PHASE 3:** [🛠 Process]

Before we start analyzing, it is necessary to make sure data is clean, free of error and in the right format. To effectively do this, we will first install and load the required packages needed for our analysis:

```{r}
# Install packages 
install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("hms")

# Load Packages
library(readr)
library(tidyverse)
library(skimr)
library(janitor)
library(dplyr)
library(lubridate)
library(hms)
```

We can then successfully load and examine the dataset:

```{r}
# Load the data set 
bike_data01 <- read_csv('random_subset_divvy_trips.csv')

# Clean the data set 
#examne dataset 
head(bike_data01)
dim(bike_data01)
colnames(bike_data01)
summary(bike_data01)
```

In the cleaning phase, we select the distinct columns and remove the unnecessary columns needed for our analysis:

```{r}
# Make sure there are no duplicate rides in data 
bike_data01 <- bike_data01 %>%
  distinct(ride_id,.keep_all=TRUE)

# Select needed columns for analysis
bike_data02 <- bike_data01 %>%
  select(member_casual, started_at, ended_at, start_station_id, end_station_id)

# Clean the data set by removing rows with missing values
bike_data02 <- bike_data02 %>%
  filter_all(any_vars(!is.na(.)))
```

## **PHASE 4:** [📊 Analyze]

The analyze phase will consist of aggregating the dataset to find meaningful trends that will be used to answer our business questions. We are looking for the differences between rider behaviours(member and casual). It will be useful to find how many participants are in each category. The code chunk below finds this information and visualizes it with a pie chart:

```{r}
# Create member_vs_casual_count variable
member_vs_casual_count <- bike_data02 %>%
  group_by(member_casual) %>%
  summarise(count = n())

# Create member_vs_casual_count pie chart
member_vs_casual_count %>%
  ggplot(aes(x = "", y = count, fill = member_casual)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Distribution of Rides by User Type",
       fill = "User Type",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "right") +
  geom_text(aes(label = scales::percent(count / sum(count)), y = count), 
            stat = "identity", position = position_stack(vjust = 0.5))
```

Now that we have sufficient information on the differences between the casual and member type of riders, we can dig deeper into our dataset and find trends concerning the rider types with respect to ; most popular usage hours, most popular usage days, most popular usage months, highest usage duration:

```{r}
# Create bike_data05 with separate start_date and start_time columns
bike_data05 <- bike_data02 %>%
  mutate(started_at = as.POSIXct(started_at, format="%Y-%m-%d %H:%M:%S"),
         start_date = as.Date(started_at),
         start_time = format(started_at, "%H:%M:%S"))

# Separate ended_at into end_date and end_time columns in bike_data05
bike_data05 <- bike_data05 %>%
  mutate(ended_at = as.POSIXct(ended_at, format="%Y-%m-%d %H:%M:%S"),
         end_date = as.Date(ended_at),
         end_time = format(ended_at, "%H:%M:%S"))

# Create start_day_of_week column
bike_data05 <- bike_data05 %>%
  mutate(start_day_of_week = weekdays(started_at))

# Plot the double line graph for frequency of rides per day 
bike_data05 %>%
  mutate(start_day_of_week = factor(start_day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  ggplot(aes(x = start_day_of_week, color = member_casual, group = member_casual)) +
  geom_line(stat = "count") +
  scale_color_manual(values = c("casual" = "red", "member" = "blue")) +
  labs(title = "Frequency of Rides by Day of the Week",
       x = "Day of the Week",
       y = "Frequency") +
  theme_minimal()

# Aggregate the frequency of rides per month
monthly_rides <- bike_data05 %>%
  group_by(member_casual, month = month(started_at, label = TRUE)) %>%
  summarise(count = n())

# Create a line chart to show the difference for casual and member for each month 
monthly_rides %>%
  ggplot(aes(x = month, y = count, color = member_casual, group = member_casual)) +
  geom_line() +
  scale_color_manual(values = c("casual" = "red", "member" = "blue")) +
  labs(title = "Frequency of Rides per Month",
       x = "Month",
       y = "Frequency") +
  theme_minimal()

# Aggregate the frequency of rides per hour of the day
hourly_rides <- bike_data05 %>%
  group_by(member_casual, hour = format(started_at, "%H")) %>%
  summarise(count = n())

# Create a line chart to illustrate the frequency of rides per hour of day
hourly_rides %>%
  ggplot(aes(x = hour, y = count, color = member_casual, group = member_casual)) +
  geom_line() +
  scale_color_manual(values = c("casual" = "red", "member" = "blue")) +
  labs(title = "Frequency of Rides per Hour of Day",
       x = "Hour of Day",
       y = "Frequency") +
  theme_minimal()

# I suspect there is more to the eye if we separate weekend and weekday data
# Aggregate the frequency of rides per hour of the day for weekdays
hourly_rides_weekdays <- bike_data05 %>%
  filter(!weekdays(started_at) %in% c("Saturday", "Sunday")) %>%  # Exclude weekends
  group_by(member_casual, hour = format(started_at, "%H")) %>%
  summarise(count = n())

# Create a line chart to illustrate the rides per hour of day for weekdays
hourly_rides_weekdays %>%
  ggplot(aes(x = hour, y = count, color = member_casual, group = member_casual)) +
  geom_line() +
  scale_color_manual(values = c("casual" = "red", "member" = "blue")) +
  labs(title = "Frequency of Rides per Hour of Day (Weekdays Only)",
       x = "Hour of Day",
       y = "Frequency") +
  theme_minimal()

# Aggregate the frequency of rides per hour of the day for weekends
hourly_rides_weekends <- bike_data05 %>%
  filter(weekdays(started_at) %in% c("Saturday", "Sunday")) %>%  # Include only weekends
  group_by(member_casual, hour = format(started_at, "%H")) %>%
  summarise(count = n())

# Create a line chart to illustrate the data
hourly_rides_weekends %>%
  ggplot(aes(x = hour, y = count, color = member_casual, group = member_casual)) +
  geom_line() +
  scale_color_manual(values = c("casual" = "red", "member" = "blue")) +
  labs(title = "Frequency of Rides per Hour of Day (Weekends Only)",
       x = "Hour of Day",
       y = "Frequency") +
  theme_minimal()


# Create a new variable 'duration' based on start_time and end_time
bike_data05 <- bike_data05 %>%
  mutate(duration = difftime(ended_at, started_at, units = "mins"))

# Define the order of days of the week
weekday_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Convert 'start_day_of_week' to a factor with the desired order
bike_data05$start_day_of_week <- factor(bike_data05$start_day_of_week, levels = weekday_order, ordered = TRUE)
# Aggregate the average duration for member and casual rides for each day of the week
average_duration_by_day <- bike_data05 %>%
  filter(duration >= 0) %>%
  group_by(member_casual, start_day_of_week) %>%
  summarise(avg_duration = mean(as.numeric(duration), na.rm = TRUE))

# Create a vertical bar chart
average_duration_by_day %>%
  ggplot(aes(x = start_day_of_week, y = avg_duration, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Duration for Member and Casual Rides by Day of the Week",
       x = "Day of the Week",
       y = "Average Duration (minutes)",
       fill = "User Type") +
  theme_minimal() +
  theme(legend.position = "right")

# Calculate the most frequent start station for both casual and member rides
most_frequent_start_station <- bike_data01 %>%
  group_by(member_casual, start_station_name) %>%
  summarise(count = n()) %>%
  arrange(member_casual, desc(count)) %>%
  slice(7)

# Create a horizontal bar chart
most_frequent_start_station %>%
  ggplot(aes(x = count, y = reorder(start_station_name, -count), fill = member_casual)) +
  geom_col() +
  labs(title = "Most Frequent Start Stations for Casual and Member Rides",
       x = "Frequency",
       y = "Start Station Name",
       fill = "User Type") +
  theme_minimal() +
  theme(legend.position = "right")
```

## PHASE 5: [📋 Share]

## PHASE 6: [🚲 Act]

-   Casual riders take less number of rides but for longer durations.
-   Casual Riders are most active on weekends, and the months of June and July.
-   Casual riders mostly use bikes for recreational purposes.

Here are my top 3 recommendations based on above key findings:

1.  Design riding packages by keeping recreational activities, weekend contests, and summer events in mind and offer special discounts and coupons on such events to encourage casual riders get annual membership.

2.  Design seasonal packages, It allows flexibility and encourages casual riders to get membership for specific periods if they are not willing to pay for annual subscription.

3.  Effective and efficient promotions by targeting casual riders at the busiest times and stations:

    -   Days: Weekends

    -   Months: February, June, and July

    -   Stations: Streeter Dr & Grand Ave, Lake Shore Dr & Monroe St, Millennium Park
