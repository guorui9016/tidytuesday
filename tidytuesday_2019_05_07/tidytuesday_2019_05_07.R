library(tidyverse)
library(countrycode)

ratio <- read_csv("tidytuesday_2019_05_07/data/student_teacher_ratio.csv")

world <- map_data("world")

# Question 1: We need more teacher in the world

ratio %>% 
  filter(indicator == "Tertiary Education") %>% 
  left_join(world, by = c("country"="region")) %>% 
  ggplot()+
  geom_map(data=world, map = world,
           aes(long, lat, group = group, map_id=region))+
  geom_map(map = world,aes(fill=student_ratio, map_id = country))+
  theme_classic()+
  scale_fill_gradient(low = "red", high = "green")+
  labs(x="", y="", title = "We need more teacher in the world", fill="Student Ratio")+
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        plot.title = element_text(size = 20, hjust=0.5))


# Question 2:  Compared tudent to teacher ratio by continent

country_continent <- codelist %>% 
  select(iso3c, country.name.en, continent)

ratio %>% 
  left_join(country_continent, by = c("country_code" = "iso3c")) %>% 
  group_by(indicator, continent) %>% 
  summarise(median = median(student_ratio, na.rm = T)) %>% 
  filter(continent != "NA") %>% 
  ggplot(aes(x=indicator, y = median, color = continent))+
  geom_point()+
  theme_classic()+
  labs(x="", y="Student to teacher ratio", color = "Continent")+
  coord_flip()

# Question 3: Student to teacher rations in global
ratio %>% 
  left_join(country_continent, by = c("country_code" = "iso3c")) %>% 
  filter(continent != "NA") %>% 
  ggplot(aes(x=indicator, y=student_ratio, color=continent)) +
  geom_boxplot()+
  scale_y_continuous(limits = c(0,75))+
  theme_classic()+
  labs(x="", y="Student Ratio", title = "Student to teacher rations in global")+
  theme(axis.text = element_text(angle = 90))

# Question 4:




