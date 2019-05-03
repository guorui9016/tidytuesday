library(tidyverse)
library(lubridate)

mp_light <- read_csv("tidytuesday_2019_04_30/data/mp_light.csv")
bird_collisions <- read_csv("tidytuesday_2019_04_30/data/bird_collisions.csv" )
bird_call <- read_table2("tidytuesday_2019_04_30/data/bird_call.csv")

####################################################################

# 1. Learn how to use month function transfer number to words.
# 2. fct_reorder and reorder function.




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
  facet_wrap(~locality)+
  theme_classic()
  

# Question 2: From the Q1 I just want know which kind of bird become to more?

q2 <- bird_stat %>% 
        count(family, year, locality) %>% 
        mutate(family = fct_reorder(family, -n)) %>%
        filter(n>30)

#### way 1

q2 %>% 
  ggplot(aes(year, n, fill = family))+
  geom_area(position = "identity", alpha = 0.75)+
  facet_wrap(~locality)+
  labs(x=" Year",
       y = "Number of collisions",
       title = "Which kind of bird come to Chicago?")+
  theme_classic()
        
#### way 2

q2 %>% 
  ggplot(aes(year, y = reorder(family, n)))+
  geom_point(aes(size = n), alpha = 0.5)+
  facet_wrap(locality~.)+
  labs(x=" Year",
       y = "Family",
       title = "Which kind of bird come to Chicago?",
       size = "Number of collisions")+
  theme_classic()
  
# Question3:

