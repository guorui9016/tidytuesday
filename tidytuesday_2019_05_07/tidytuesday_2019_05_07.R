library(tidyverse)
library(countrycode)

ratio <- read_csv("tidytuesday_2019_05_07/data/student_teacher_ratio.csv")

world <- map_data("world")

# Question 1: We need more teacher in the world


## should not use left_join because it will lost some information of countrys.
ratio %>% 
  filter(indicator == "Tertiary Education") %>% 
  left_join(world, by = c("country"="region")) %>% 
  ggplot()+
  geom_polygon(aes(long, lat, group = group, fill = student_ratio)  )+
  theme_classic()

  
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
  theme(axis.text = element_text(angle = 45, hjust=1))

# Question 4: Do more people want to be a teacher?

ratio %>% 
  left_join(country_continent, by=c("country_code" = "iso3c")) %>% 
  filter(continent != "NA") %>%
  group_by(year, continent, indicator) %>% 
  summarise(medi = median(student_ratio, na.rm = T)) %>% 
  ggplot(aes(x=year, y=medi, color=continent))+
  geom_point()+
  geom_line()+
  facet_wrap(~indicator,scales = "free_x")+
  theme_classic()+
  labs(x="Years", y="Student ratio", color="Continent",
       title = "Do more people want to be a teacher?")

#### Way 2:

ratio %>% 
  left_join(country_continent, by=c("country_code" = "iso3c")) %>% 
  filter(continent != "NA") %>%
  group_by(year, continent, indicator) %>% 
  summarise(medi = median(student_ratio, na.rm = T)) %>% 
  ggplot(aes(x=year, y=medi, color=continent))+
  geom_point()+
  geom_line()+
  facet_grid(continent~indicator)+
  theme_classic()+
  labs(x="Years", y="Student ratio", color="Continent",
       title = "Do more people want to be a teacher?")

# Question 5: 

ratio %>% 
  filter(indicator %in% c("Primary Education", "Secondary Education", "Tertiary Education"),
         year == "2017",
         str_detect(country,"countries")) %>% 
  ggplot(aes(x=student_ratio,y=reorder(country, -student_ratio), color=indicator))+
  geom_point(size = 3)+
  theme_light()+
  labs(x="Student ratio", y="", title = "Low income contries need more teacher", color="Indicator")



