library(tidyverse)

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
