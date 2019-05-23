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


jobs_gender %>% 
  select(major_category, total_earnings, total_earnings_male, total_earnings_female) %>%
  mutate(wage_percent_avg_m  = total_earnings_male/total_earnings,
         wage_percent_avg_f = total_earnings_female/total_earnings) %>% 
  ggplot()+
  geom_jitter(aes(x=major_category, y =wage_percent_avg_m),color = "blue", alpha = 0.6)+
  geom_jitter(aes(x=major_category, y =wage_percent_avg_f), color = "green", alpha = 0.6)+
  geom_hline(yintercept = 1, color = "white")+
  theme_classic()+
  labs(x = "", y="Precent of average wage", title = "Women get low income than man")+
  coord_flip()

jobs_gender %>% 
  select(major_category, total_earnings, total_earnings_male, total_earnings_female) %>%
  mutate(wage_percent_avg_m  = total_earnings_male/total_earnings,
         wage_percent_avg_f = total_earnings_female/total_earnings) %>% 
  select(major_category, wage_percent_avg_m,wage_percent_avg_f) %>% 
  gather(key = "gender", value = "value", c("wage_percent_avg_m","wage_percent_avg_f")) %>% 
  ggplot(aes(x=major_category, y= value, color = gender))+
  geom_jitter(alpha=0.5)+
  geom_hline(yintercept=1, color = "white")+
  theme_classic()+
  labs(x="", y="Percent of average wage", title ="Women get low income than man")+
  scale_color_hue(labels=c("Female", "Male"))+
  coord_flip()
  
# Question 5: Women sacrifice for family?

employed_gender %>% 
  gather(key = "group", value = "value", total_full_time:part_time_male) %>% 
  separate(group, c("job_type","time","gender"), "_") %>% 
  filter(job_type!="total") %>% 
  ggplot(aes(x=year,  y = value, fill = gender))+
  geom_col(position = "dodge") +
  facet_wrap(~job_type)

employed_gender %>% 
  gather(key = "group", value = "value", total_full_time:part_time_male) %>% 
  separate(group, c("job_type","time","gender"), "_") %>% 
  filter(job_type!="total") %>% 
  ggplot(aes(x=year,  y = value, color = gender))+
  geom_line() +
  facet_wrap(~job_type, labeller = c("Full time", "Part time") )+
  theme_classic()+
  labs(x="Year", 
       y="Work hours", 
       title = "Women sacrifice for family?",
       color = "Gender")





