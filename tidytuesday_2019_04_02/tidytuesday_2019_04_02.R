#### 1. learn use dmy_hms and what diff between week and wday.


#### 2. Split string by " " in data frame ??

library(tidyverse)
library(lubridate)

bike_raw <- read_csv("tidytuesday_2019_04_02/data/bike_traffic.csv")
  
bike <- bike_raw %>% 
  mutate(date = dmy_hms(date)) %>% 
  mutate(weekday = wday(date, label = T), 
         year = year(date)) %>% 
  filter(weekday != "NA")

# Question 1: Which day is a busy day?

bike %>% 
  group_by(weekday, year) %>% 
  filter(year !=2019 ) %>% 
  summarise(sum = sum(bike_count, na.rm = T)) %>% 
  ggplot(aes(x= weekday, y = sum, group = year, color= as.factor(year)))+
  geom_line(size = 2, alpha = 0.5)+
  geom_point(shape = 20, size  = 5,alpha = 0.5)+
  labs(x="Week", y = "Number of bikes", title = "Do people like ride bike now?", color = "Year")+
  theme_classic()
  
# Question 2: Busy road?

q2 <- bike %>% 
  group_by(weekday, crossing) %>% 
  summarise(sum = sum(bike_count, na.rm = T)) %>% 
  ggplot(aes(x=weekday, y=sum, group = crossing, color = crossing))+
  geom_line(size =2 , alpha = 0.5)+
  geom_point(shape = 20, size  = 5,alpha = 0.5)+
  labs(x="Week", y = "Number of bikes", title = "How busy of road", color = "Road")+
  theme_classic()

q2
  

# Question 3: 