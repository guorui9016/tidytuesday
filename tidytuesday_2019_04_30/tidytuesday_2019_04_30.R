library(tidyverse)
library(lubridate)
library(collapsibleTree)

mp_light <- read_csv("tidytuesday_2019_04_30/data/mp_light.csv")
bird_collisions <- read_csv("tidytuesday_2019_04_30/data/bird_collisions.csv" )
bird_call <- read_table2("tidytuesday_2019_04_30/data/bird_call.csv")

####################################################################

# 1. Learn how to use month function transfer number to words.
# 2. fct_reorder and reorder function.
# 3. use collapsibleTree pack to draw a tree.




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

#### Way2 

bird_stat %>% 
  count(year, month, locality) %>% 
  ggplot(aes(x =  year, y = n, fill = month))+
  geom_col()+
  labs(x="Years", y="Number of collisions", title = "Does bird happy now?", fill="Month")+
  facet_wrap(~locality)+
  theme_classic()

#### Way3

bird_stat %>% 
  count(year, month, locality) %>% 
  ggplot(aes(x =  year, y = n, color = locality))+
  geom_smooth(alpha=0.5, se = F )+
  labs(x="Years", y="Number of collisions", tittle = "Does bird happy now?")+
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
       title = "Chicago bird collisions")+
  theme_classic()

#### way 2

q2 %>% 
  ggplot(aes(year, y = reorder(family, n)))+
  geom_point(aes(size = n), alpha = 0.5)+
  facet_wrap(locality~.)+
  labs(x=" Year",
       y = "Family",
       title = "Chicago bird collisions",
       size = "Number of collisions")+
  theme_classic()

rm(list = "q2")

# Question 3:

q3 <- bird_collisions %>% 
  mutate_if(is.character, as.factor) %>% 
  count(family, genus, species)

q3_1 <- bird_collisions %>% 
  mutate_if(is.character, as.factor) %>% 
  group_by(family, genus, species ) %>% 
  summarise(n = n()) %>% 
  ungroup()

collapsibleTreeSummary(q3, hierarchy = c("family", "genus", "species"),
                       attribute = "n",
                       nodeSize = "n",
                       collapsed = F)

rm(list = c("q3","q3_1"))

# Question 4: Bird window collisions by habital.

bird_stat %>% 
  count(year, habitat) %>% 
  ggplot(aes(year, n, col = habitat))+
  geom_line(size = 1, alpha = 0.6)+
  labs(x="Year", y="Number of collisions", title = "Bird window collosions by habital",col = "Habitat")+
  theme_classic()

# Question 5: Dose light effect bird collision?



light <- mp_light %>% 
  mutate(year=year(date)) %>% 
  group_by(year) %>% 
  summarise(sum_light = sum(light_score)) %>% 
  inner_join(bird_stat, by= "year") %>% 
  count(year, sum_light, locality)

light %>% 
  ggplot(aes(year,n))+
  geom_bar(stat="identity")+
  geom_line(aes(year, sum_light), color = "red")+
  facet_wrap(~locality)+
  theme_classic()+
  labs(x="Year", y="Number of collision", title="Does light effect bird collision?")





