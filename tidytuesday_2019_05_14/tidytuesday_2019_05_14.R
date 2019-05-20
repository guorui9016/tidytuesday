library(tidyverse)
library(lubridate)
library(magrittr)

archive <- read_csv("tidytuesday_2019_05_14/data/archive.csv")
chemistry <- read_csv("tidytuesday_2019_05_14/data/Chemistry publication record.csv")
medicine <- read_csv("tidytuesday_2019_05_14/data/Medicine publication record.csv")
nobel_winner_all_pubs <- read_csv("tidytuesday_2019_05_14/data/nobel_winner_all_pubs.csv")
nobel_winner <- read_csv("tidytuesday_2019_05_14/data/nobel_winners.csv")
physics <- read_csv("tidytuesday_2019_05_14/data/Physics publication record.csv")
prize <- read_csv("tidytuesday_2019_05_14/data/Prize-winning paper record.csv")


# Queston 1: Still man's world?

nobel_winner %>% 
  filter(gender!="NA") %>% 
  ggplot(aes(x=category,fill=gender))+
  geom_bar(position = "dodge")+
  coord_flip()+
  theme_light()+
  labs(x="", y= "Number of Nobel Laureates", title = "Still man's world?")

# Question 2: Following Q1, I want know should world been changed recently?

nobel_winner %>% 
  filter(gender!="NA") %>% 
  count(category, gender, prize_year) %>% 
  mutate(decade = (prize_year %/% 10) * 10 ) %>% 
  ggplot(aes(x=decade, y=n, fill=gender))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_wrap(category~.)+
  theme_light()+
  labs(x="decade", y="Number of Nobel Laureates", fill="Gender",
     title = "Still man's world?")
  
# Question 3: Which country is the winer?

nobel_winner %>% 
  count(organization_country) %>% 
  filter(organization_country!="NA",
         n > 2) %>% 
  ggplot(aes(x=reorder(organization_country, n), y = n ))+
  geom_col()+
  coord_flip()

# Question 4: Age when get the prize.

nobel_winner %>% 
  mutate(age = prize_year - year(birth_date)) %>% 
  count(age) %>% 
  ggplot(aes(x=age, y=n)) +
  geom_line()+
  theme_light()+
  labs(x="Age",
       y= "Number of Scitentist",
       title = "Too old to get Nobel prize")

# Question 5: 

nobel_winner_all_pubs %>% 
  filter(is_prize_winning_paper=="YES",
         !is.na(journal)) %>%
  count(journal, category) %>%
  group_by(category) %>%
  arrange(desc(n)) %>% 
  slice(1:15) %>%
  ggplot(aes(x=fct_reorder(journal, -n), y = n ))+
  geom_col()+
  # coord_flip()+
  facet_wrap(~category, scales = "free")

# Question 6: Wnat to know status by category 

nobel_winner %>% 
  ggplot(aes(x=category, y=prize_year)) +
  geom_violin()+
  theme_light()+
  labs(x="", y="Year", title = "Economics become more importent now!")+
  theme(axis.text.x = element_text(angle = 90))
           
#### Way 2

nobel_winner %>% 
  ggplot(aes(x=category, y=prize_year,color = gender, fill = gender)) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 2)+
  theme_light()+
  labs(x="", y="Year", title = "Lot of people work on chemistry in last few years!")+
  theme(axis.text.x = element_text(angle = 90))

# Question 7: Want to know prize winer by birth country.




