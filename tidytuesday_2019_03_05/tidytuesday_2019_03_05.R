library(tidyverse)

earnings_female <- read_csv("tidytuesday_2019_03_05/data/earnings_female.csv")
employed_gender <- read_csv("tidytuesday_2019_03_05/data/employed_gender.csv")
jobs_gender <- read_csv("tidytuesday_2019_03_05/data/jobs_gender.csv")

# Question 1: Does women earn become to more?

earnings_female %>% 
  filter(group=="Total, 16 years and older") %>%
  ggplot(aes(x=Year, y=percent)) +
  geom_line(color="green", size = 2)+
  theme_light()+
  labs(x="Year",
       y="Percent",
       title = "Women earn more than before")

# Question 2: 

earnings_female %>% 
  filter(group!="Total, 16 years and older") %>% 
  ggplot(aes(x=Year,y=percent,color=group))+
  geom_line(size =1)+
  theme_light()+
  labs(x="Year",
       y="Percent",
       title = "More women go to work")

  
