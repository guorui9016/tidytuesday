#### 1. learn use dmy_hms and what diff between week and wday.


#### 2. Split string by " " in data frame ??
Sys.setlocale("LC_TIME", "English")

library(tidyverse)
library(lubridate)

bike <- read_csv("tidytuesday_2019_04_02/data/bike_traffic.csv", col_types = "cccdd") %>% 
  mutate(date = mdy_hms(date)) %>% 
  mutate(day = wday(date, label = T),
         year = year(date))


# Question 1: Which day is a busy day?

bike %>% 
  group_by(day, year) %>% 
  filter(year !=2019 & year!= 2013 ) %>% 
  summarise(sum = sum(bike_count, na.rm = T)) %>% 
  ggplot(aes(x= day, y = sum, group = year, color= as.factor(year)))+
  geom_line(size = 2, alpha = 0.5)+
  geom_point(shape = 20, size  = 5,alpha = 0.5)+
  labs(x="Week", y = "Number of bikes", title = "Do people like ride bike now?", color = "Year")+
  theme_classic()
  
# Question 2: Busy road?

bike %>% 
  group_by(day, crossing) %>% 
  summarise(sum = sum(bike_count, na.rm = T)) %>% 
  ggplot(aes(x = ordered(day, levels = c( "Mon","Tue","Wed","Thu","Fri","Sat","Sun")), 
             y=sum, 
             group = crossing, 
             color = crossing))+
  geom_line(size =2 , alpha = 0.5)+
  geom_point(shape = 20, size  = 5,alpha = 0.5)+
  labs(x="Week", y = "Number of bikes", title = "How busy of road", color = "Road")+
  theme_classic()


# Question 3: I want see what diff between weekday and weekend by diff road.

q3 <- bike %>% 
  mutate(day_type = if_else(day %in% c("Sat", "Sun"), "weekdend", "weekday")) %>% 
  group_by(day, day_type, crossing) %>% 
  summarise(sum = sum(bike_count, na.rm = T)) %>% 
  ggplot(aes(x =crossing , y =sum ,  fil= day_type))+
  geom_bar(stat = "identity", alpha = 0.6)

q3
