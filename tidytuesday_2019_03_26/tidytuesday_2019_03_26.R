library(tidyverse)
library(lubridate)
library(zipcode)

pets <- read_csv("tidytuesday_2019_03_26/data/seattle_pets.csv") %>% 
  mutate(issue_date = mdy(license_issue_date),
         animals_name = as.factor(animals_name))

# Question 1: Cat VS Dog (饼图)

pets %>% 
  filter( species %in% c("Cat", "Dog")) %>% 
  count(species) %>% 
  ggplot(aes(x=species, y=n, fill=species))+
  geom_bar(stat = "identity")+
  coord_polar()+
  geom_text(aes(label=species))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = 'none')+
  labs(title = "Cat vs Dog")

# Question 2: Top 10 name of cat and dog

pets %>% 
  filter(species %in% c("Dog", "Cat"),
         animals_name!="NA") %>%
  mutate(animals_name = fct_infreq(animals_name)) %>% 
  count(animals_name, species) %>% 
  group_by(species) %>%
  top_n(n=10, wt=n) %>% 
  ggplot(aes(x=animals_name, y=n))+
  geom_col()+
  facet_wrap(~species, scales = "free")+
  labs(x = "Name",
       y = "Number of name", 
       title = "Top 10 of Name")+
  theme_classic()+
  theme(axis.text = element_text(angle = 90))

# Question 3: Cat city or dog city?

data("zipcode")

# seattle <- map_data("Seattle") 

pets %>% 
  filter(zip_code!="NA",
         species %in% c("Dog", "Cat")) %>% 
  count(species, zip_code) %>% 
  inner_join(zipcode, by=c("zip_code" = "zip")) %>% 
  ggplot(aes(x=longitude, y=latitude))+
  geom_polygon()+
  geom_point(aes(size=n))


# Question 4: Pet number growing in Seattle?

pets %>% 
  mutate(year = year(issue_date)) %>% 
  count(species, year) %>% 
  filter(year > 2011) %>% 
  ggplot(aes(x=year, y=n, color = species))+
  geom_line(size = 2, alpha=0.8)+
  labs(x="Year",
       y="Number of Pet",
       title = "Do human feel alone?",
       color = "Pet")+
  theme_classic()

# Question 5: Which name is good for both pet?

pets %>% 
  filter(animals_name!="NA", 
         species %in% c("Dog", "Cat")) %>% 
  count(animals_name, species) %>% 
  group_by(animals_name) %>% 
  mutate(index = n()) %>% 
  filter(index >1) %>% 
  mutate(index = sum(n)) %>% 
  arrange(-index) %>% 
  ungroup() %>%
  top_n(n=40, wt=index) %>% 
  mutate(animals_name = fct_reorder(animals_name, -index)) %>% 
  ggplot(aes(x=animals_name, y=n, fill=species))+
  geom_bar(stat="identity", width = 0.7)+
  labs(x="Pet Name",
       y="Number of pet",
       title = "Good name for both dog and cat",
       fill = "Pet")+
  theme_classic()+
  theme(axis.text = element_text(angle = 90))