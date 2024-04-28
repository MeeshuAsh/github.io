---
title: "DA_CapstoneProject"
author: "Ashutosh Singh"
date: "2024-04-09"
---

We will be following 6-steps approach to analyze the data that would be presented to the Key Stakeholders in order to make informed Business Decisions.

Those 6-steps are as follows:<br>
1. Ask<br>
2. Prepare<br>
3. Process<br>
4. Analyze<br>
5. Share<br>
6. Act<br>

The Key Stakeholders would be:<br>
**Urška Sršen**: Bellabeat’s cofounder and Chief Creative Officer,<br>
**Sando Mur**: Mathematician and Bellabeat’s cofounder; key member of the Bellabeat executive team, and<br>
**Bellabeat marketing analytics team**<br>


## Background of the company
Bellabeat is a high-tech company which was founded in 2013 that manufactures health-focused smart products. Bellabeat has grown rapidly and quickly positioned itself as a tech-driven wellness company for women. They specialize in 5 home grown products which are Bellabeat app, Leaf, Time, Spring and Bellabeat memberships.<br>

## Ask Phase
Analyze smart device usage data in order to gain insight into how consumers use smart devices.<br>

## Prepare Phase
Stakeholder encourages to use public data that explores smart device users’ daily habits. FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius): This Kaggle data set contains personal fitness tracker from thirty fitbit users. Thirty eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring. It includes information about daily activity, steps, and heart rate that can be used to explore users’ habits.<br>
During this phase, I encountered few limitations while working with the datasets, which are briefed below:<br>
a. Data is unclear if it included all genders and diversity<br>
b. The dataset of 33 users' is fairly small and we might encounter sampling bias<br>
c. The dataset doesn't contain much of demographic details<br>
d. The survey was collected for short period of time (~2 months), which tells that the data may not be latest<br>

## Process Phase
To be able to present my analysis, I chose R to clean, process and analyze the data.

#### Environment setup in R

```{r packages}
install.packages("tidyverse")
install.packages("skimr")
install.packages("ggplot2")
install.packages("lubridate")
```

#### Loading the packages
```{r library}
library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)
```

#### Data Import
```{r data_import}
daily_activity <- read_csv ("dailyActivity_merged.csv")
daily_Steps <- read_csv ("dailySteps_merged.csv")
hourly_Steps <- read_csv("hourlySteps_merged.csv")
hourly_Calories <- read_csv ("hourlyCalories_merged.csv")
```

#### Check the Structure of the data
```{r structure}
str(daily_activity)
str(daily_Steps)
str(hourly_Steps)
str(hourly_Calories)
```

#### View the data
```{r view}
head(daily_activity)
head(daily_Steps)
head(hourly_Steps)
head(hourly_Calories)
```

#### Verify the column names
```{r verify}
colnames(daily_activity)
colnames(daily_Steps)
colnames(hourly_Steps)
colnames(hourly_Calories)
```

#### Check for Duplicate Values
```{r check}
sum(duplicated(daily_activity))
sum(duplicated(daily_Steps))
sum(duplicated(hourly_Steps))
sum(duplicated(hourly_Calories))
```

#### Remove missing values (if any)
```{r missing}
daily_activity <- daily_activity %>%
  drop_na()
daily_Steps <- daily_Steps %>%
  drop_na()
hourly_Steps <- hourly_Steps %>%
  drop_na()
hourly_Calories <- hourly_Calories %>%
  drop_na()
```

## Analyze and Share Phase

#### Summarize daily activity

```{r summarize}
daily_activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes, Calories) %>%
  summary()
```
  

<br> Outcome from the summary:<br>
1. Average Total Steps taken is 7638<br>
2. Average Sedentary time is 991 minutes, which is higher than expected. Recommend user to lower the sedentary time.<br>

#### Total Steps vs Calories
```{r totalsteps}
ggplot(data = daily_activity, aes(x = TotalSteps, y = Calories)) + 
  geom_point() + geom_smooth() + labs(title ="Total Steps vs. Calories")
```
![image](https://github.com/MeeshuAsh/github.io/assets/168366387/0a1000c1-21a2-4707-bf70-985cc9316048)

#### Hourly steps in a day

I noticed that the Date and Time are merged in 1 column in the dataset, we will need to seperate date and time and 2 columns for this analysis<br>

The below script will standardize the date and time<br>
```{r hourlysteps}
hourly_Steps<- hourly_Steps %>% 
  rename(date_time = ActivityHour) %>% 
  mutate(date_time = as.POSIXct(date_time, format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))
```

<br> Then we seperate date and time in 2 columns using below script:<br>

```{r steps}
hourly_Steps <- hourly_Steps %>%
  separate(date_time, into = c("date", "time"), sep= " ") %>%
  mutate(date = ymd(date)) 

head(hourly_Steps)
```

Visualization of Hourly Steps in a day through graph
```{r viz}
hourly_Steps %>%
  group_by(time) %>%
  summarize(average_steps = mean(StepTotal)) %>%
  ggplot() +
  geom_col(mapping = aes(x=time, y = average_steps, fill = average_steps)) + 
  labs(title = "Hourly steps in a day", x="", y="") + 
  scale_fill_gradient(low = "blue", high = "green")+
  theme(axis.text.x = element_text(angle = 90))
```
<br>

#### Observation
People are more active between 8 AM to 5 PM as they walk more steps, while 3 PM being rest time for most of them. 

#### Hourly Calories in a day

The below script will standardize the date and time<br>
```{r calories}
hourly_Calories<- hourly_Calories %>% 
  rename(date_time = ActivityHour) %>% 
  mutate(date_time = as.POSIXct(date_time, format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))
```

<br> Then we seperate date and time in 2 columns using below script:<br>

```{r hourlycalories}
hourly_Calories <- hourly_Calories %>%
  separate(date_time, into = c("date", "time"), sep= " ") %>%
  mutate(date = ymd(date)) 

head(hourly_Calories)
```

<br> Visualization of Hourly Steps in a day through graph
```{r vizcalories}
hourly_Calories %>%
  group_by(time) %>%
  summarize(average_calories = mean(Calories)) %>%
  ggplot() +
  geom_col(mapping = aes(x=time, y = average_calories, fill = average_calories)) + 
  labs(title = "Hourly Calories in a day", x="", y="") + 
  scale_fill_gradient(low = "yellow", high = "red")+
  theme(axis.text.x = element_text(angle = 90))
```
<br>

#### Observation
More Calories are burnt between 8 AM to 5 PM as they walk more steps, while 3 PM being rest time for most of them.


## Act Phase

Here are some recommendations for company to utilize the data in improving their marketing strategy and take better business decisions.<br>
1. Notifications in the app or device. For example: Remind the customer to drink water, go for a short walk, overall daily progress,  etc.<br>
2. Feature on app to be able to create a community and share the progress with others (avoid personal details). This might encourage people to perform better in the areas they're falling behind.<br>
3. Ranking or VIP level status based on customer's achievements.<br>



Thanks for reading
