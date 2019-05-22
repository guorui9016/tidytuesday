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
  theme_minimal()+
  labs(x="Year",
       y="Percent",
       title = "More women go to work")

# Question 3:

jobs_gender %>% filter(!is.na(wage_percent_of_male)) %>%
  mutate(major_category = fct_reorder(major_category, wage_percent_of_male)) %>% 
  ggplot(aes(x = major_category, y = wage_percent_of_male))+
  geom_jitter(show.legend = F,aes(color=major_category), alpha = 0.6)+
  geom_boxplot(alpha =0.3, color ="gray30")+
  coord_flip()+
  theme_light()+
  labs(x="",
       y="Wage precent of male",
       title = "Women in the workforce")+
  scale_color_viridis_d(option = "B", begin = 0.6, end = 0.85)+
  theme(plot.background = element_rect(fill = "black"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        panel.background = element_rect(fill = "black"),
        panel.grid = element_blank())
  

# Question 4: income by gender 

# jobs_gender %>%
#   select(major_category, total_earnings, total_earnings_male, total_earnings_female) %>%
#   mutate(major_category = fct_reorder(major_category, total_earnings)) %>%
#   reshape(idvar = "major_category",
#           varying = c("total_earnings","total_earnings_male","total_earnings_female"),
#           v.names  = c("income"),
#           timevar = "group",
#           times = c("total_earnings","total_earnings_male","total_earnings_female"),
#           new.row.names = 1:10000,
#           direction = "long") %>%
#   ggplot(aes(aes(x= major_category, y = income, color = group)))+
#   geom_point()

jobs_gender %>%
  select(major_category, total_earnings, total_earnings_male, total_earnings_female) %>%
  mutate(major_category = fct_reorder(major_category, total_earnings)) %>%
  ggplot(aes(x=major_category))+
  geom_jitter(aes(y=total_earnings_male),color = "blue", alpha = 0.4)+
  geom_jitter(aes(y=total_earnings_female),color = "red", alpha = 0.4)+
  theme_light()+
  coord_flip()+
  labs(x="",y= "Income", title = "Income by gender")
  
  
