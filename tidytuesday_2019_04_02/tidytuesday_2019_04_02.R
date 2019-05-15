#### 1. learn use dmy_hms and what diff between week and wday.


#### 2. Split string by " " in data frame ??
Sys.setlocale("LC_TIME", "English")

library(tidyverse)
library(lubridate)

bike <- read_csv("tidytuesday_2019_04_02/data/bike_traffic.csv", col_types = "cccdd") %>% 
  mutate(date = mdy_hms(date)) %>% 
  mutate(day = wday(date, label = T),
         year = year(date),
         time = hour(date))


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


# Question 3: Which direction by diff road

bike %>% 
  filter(crossing!="Sealth Trail") %>% 
  group_by(day, crossing, direction) %>% 
  summarise(sum=sum(bike_count, na.rm = T)) %>% 
  ggplot(aes(x=day, y=sum))+
  geom_col(aes(fill = direction), position = "dodge")+
  facet_wrap(crossing~.)+
  labs(x="Day", 
       y="Number of bike",
       title = "Which direction by diff road?",
       fill = "Direction")+
  theme(plot.title = element_text(hjust = 0.5))
  theme_classic()

# Question 4: When they ride bike?
  
bike %>% 
  filter(crossing !="Sealth Trail", year!= 2019, year!=2013) %>% 
  mutate(day_type = if_else(day %in% c("Sat", "Sun"), "Weekend", "Weekday")) %>% 
  group_by(day_type, time, crossing, year) %>% 
  summarise(sum = sum(bike_count, na.rm = T)) %>% 
  ggplot(aes(x=time, y=sum))+
  geom_line(aes(group = year, color = as.factor(year)))+
  facet_grid(day_type~crossing)+
  labs(x=" Time",
       y="Number of bike",
       group="Year",
       title = "When they ride bike")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme_classic()

#### Way 2

bike %>% 
  filter(crossing !="Sealth Trail", year!= 2019, year!=2013) %>% 
  mutate(day_type = if_else(day %in% c("Sat", "Sun"), "Weekend", "Weekday")) %>% 
  group_by(day_type, time, crossing, year) %>% 
  summarise(sum = sum(bike_count, na.rm = T)) %>% 
  ggplot(aes(x = year, y = time)) + 
  geom_tile(aes(fill = sum))+
  facet_grid(day_type~crossing)