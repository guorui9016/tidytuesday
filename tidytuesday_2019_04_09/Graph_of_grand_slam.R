library(tidyverse)
library(lubridate)
# load data

gs <- read_csv("tidytuesday_2019_04_09/data/grand_slams.csv")
gs_time <- read_csv("tidytuesday_2019_04_09/data/grand_slam_timeline.csv")
players <- read_csv("tidytuesday_2019_04_09/data/player_dob.csv") 


##############################################################
#                                                            #
# Question 1: Which tournaments is the bigest tournament？   #
#                                                            #
##############################################################

# Clean data
gs_time_c <- gs_time %>% 
  na.omit() %>% 
  mutate(year = as.factor(year)) %>% 
  filter(outcome != "Absent")

# Way 1
tour_player <- gs_time_c %>% 
  group_by(tournament) %>% 
  summarize(num_play = n())

q1_plot_1 <- ggplot(tour_player, aes(x = tournament, y = num_play))+
  geom_bar(stat = "identity")

q1_plot_1 

# Way 2

q1_plot_2 <- ggplot(gs_time_c, aes(x = tournament, fill = tournament))+
  geom_bar()

q1_plot_2

# Extra practice: Which tournaments is the bigest tournament by decade？

tour_player_year <- gs_time_c %>% 
  mutate(year = as.numeric(as.character(year)),
         decade = year - year%%10)
  
q1_plot_3 <- ggplot(tour_player_year, aes(x = as.factor(decade), fill = tournament))+
  geom_bar( position = position_stack(reverse = T))+
  labs(x = "Decade", y = "Number of players") +
  coord_flip()
q1_plot_3



##############################################################
#                                                            #
# Quesetion 2: Distribution of age by each 3 years.          #
#                                                            #
##############################################################


player_age <- players %>% 
  na.omit() %>% 
  mutate(age_year = age%/%365,
         age_group = paste((age_year%/%3)*3, "-",(age_year%/%3)*3+2 ))
  
q2_plot_1 <- ggplot(player_age, aes(x = as.factor(age_group), fill = age_group))+
  geom_bar()+
  labs(title = "Distribution of age by every 3 years",
      x="Age", 
      y = "Number of players",
      fill = "Age Group")

q2_plot_1

# Extra practice: The distribution of current age of the winners in last 5 years.

age_gs_time <- left_join(gs_time, players, by = c("player" = "name")) %>% 
  mutate(real_age = floor(decimal_date(Sys.Date())-decimal_date(as.Date(date_of_birth))),
         age_group = paste((real_age%/%3)*3, "-",(real_age%/%3)*3+2)) %>% 
  filter(outcome =="Won",
         year > (max(year)-5)) %>% 
  group_by(age_group) %>% 
  summarise(counts= n())
  

q2_plot_2 <- q2_plot_1 <- ggplot(player_age, aes(x = as.factor(age_group), fill = age_group))+
  geom_bar()+
  labs(title = "Distribution of age by every 3 years & current age of the winners in last 5 years",
       x="Age", 
       y = "Number of players",
       fill = "Age group")+
  geom_point(data = age_gs_time, aes(x = age_group, y = counts, size = age_group, color = age_group))+ 
    labs(color = "Current age", size = "Current age")
q2_plot_2


##############################################################
#                                                            #
# Quesetion 3: Top 20 players                                #
#                                                            #
##############################################################

top_player <- gs %>% 
  group_by(name, gender) %>% 
  summarise(num_win = n()) %>% 
  arrange(desc(num_win)) %>% 
  head(n =20) 

q3_plot_1 <- ggplot(top_player, aes(x = name, y = num_win, fill = name))+
  geom_bar(stat = "identity")
q3_plot_1

# I want reorder  X axis depend on Y axis

q3_plot_2 <- ggplot(top_player, aes(x = reorder(name, -num_win), y = num_win, fill = name))+
  geom_bar(stat = "identity")
q3_plot_2

# Extra practice: Use first name, and split to two plots by gender.

top_player_firstname <- top_player %>% 
  mutate(firstname = sapply(strsplit(as.character(name), " "), "[", 1))

q3_plot_3 <- ggplot(top_player_firstname, aes(x = reorder(firstname, -num_win), y = num_win, fill = firstname))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=firstname), angle = 90, hjust = 2)+
  ggtitle("The best top 20 players")+
  facet_grid(~gender)+
  theme(axis.text.x = element_text(size = 10, angle = 90))  

q3_plot_3

q3_plot_4 <- ggplot(top_player_firstname, aes(x = reorder(firstname, -num_win), y = num_win, color = gender, size = num_win))+
  geom_point()+
  geom_text(aes(label=firstname), angle = 90, hjust = 2)+
  ggtitle("The best top 20 players")+
  theme(axis.text.x = element_text(size = 10, angle = 90))  

q3_plot_4

##############################################################
#                                                            #
# Quesetion 4: The performances of top 10 players            #
#                                                            #
##############################################################

top10 <- top_player_firstname %>% 
  head(n=10) %>% 
  select("name","firstname")

temp <- inner_join(gs, top10, by = "name") %>% 
  select(-"grand_slam")

age_gs_q4 <- inner_join(temp, players, by = "name") %>% 
  mutate(age = floor(decimal_date(as.Date(tournament_date))-decimal_date(as.Date(date_of_birth)))) %>% 
  select("name", "rolling_win_count", "gender","firstname","age")
  

q4_plot_1 <- ggplot(age_gs_q4, aes(x= age, y = rolling_win_count))+
  geom_step(aes(color = name))+
  facet_grid(~gender)
  
q4_plot_1

