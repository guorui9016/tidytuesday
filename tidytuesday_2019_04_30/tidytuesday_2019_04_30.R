library(tidyverse)
library(lubridate)

mp_light <- read_csv("tidytuesday_2019_04_30/data/mp_light.csv")
bird_collisions <- read_csv("tidytuesday_2019_04_30/data/bird_collisions.csv" )
bird_call <- read_table2("tidytuesday_2019_04_30/data/bird_call.csv")

# In Q1, I learn how to use month function transfer number to words.

# Question 1: Status of bird collision data


bird_stat <- bird_collisions %>% 
  mutate(month = month(date, label = T), 
         year = year(date)) 


bird_stat %>% 
  count(year, month, locality) %>% 
  ggplot(aes(month, n, fill=year))+
  geom_col()+
  coord_flip()+
  labs(x=" Month",
       y = "Number of collisions",
       title = "Does bird happy now?")+
  facet_wrap(~locality)

# Question2: From the Q1 I just want know which kind of bird become to more?





