library(tidyverse)

games <- read_csv("tidytuesday_2019_03_12/data/board_games.csv")

# Question 1: Game market keep growing?

games %>% 
  count(year_published) %>% 
  ggplot(aes(x=year_published,y = n ))+
  geom_line()+
  labs(x="Year", y="Nmuber of games", title = "Game market keep growing")+
  theme_classic()

#### Way 2 

games %>% 
  select(year_published) %>% 
  ggplot()+
  geom_density(aes(x =year_published))+
  labs(x="Year", y="Nmuber of games", title = "Game market keep growing")+
  theme_classic()
  
# Question 2: More good game?

games %>% 
  select(year_published, average_rating) %>% 
  mutate(decade = (year_published%/%10) * 10 ) %>% 
  ggplot()+
  geom_boxplot(aes(x=decade, y = average_rating, group = decade))+
  labs(x="Decade", y= "Average rating", title = "We live in a game word")+
  theme_classic()

# Question 3: Play with firend?

games %>% 
  select(year_published, min_players) %>% 
  filter((min_players!=0)) %>% 
  mutate(min_players = as.factor(min_players)) %>% 
  ggplot(aes(x=year_published, fill=min_players ))+
  geom_density(alpha=0.5)+
  theme_classic()+
  labs(x="Year", y="Number of game", title = "Play with friend")

#### Way 2:

games %>% 
  count(year_published, min_players) %>% 
  filter((min_players!=0)) %>% 
  mutate(min_players = as.factor(min_players)) %>% 
  ggplot(aes(x=year_published, y =n, color= min_players ))+
  geom_line(size=1)+
  theme_classic()+
  labs(x="Year", y="Number of game", title = "Play with friend")

# Question 4: kids friendly now?

games %>% 
  filter(!min_age %in% c(0,42)) %>% 
  count(year_published, min_age) %>% 
  mutate(min_age = as.factor(min_age)) %>% 
  ggplot(aes(x=year_published,y=n, color=min_age))+
  geom_line(size = 1)+
  theme_classic()+
  labs(x="Year", y="Number of game", title = "Age target of game",color = "Age")

### Way 2:

games %>% 
  filter(!min_age %in% c(0,42)) %>% 
  count(year_published, min_age) %>% 
  mutate(min_age = as.factor(min_age)) %>% 
  ggplot(aes(x=year_published,y=n,color=min_age))+
  geom_point(aes(size = n), alpha = 0.5)+
  theme_classic()+
  labs(x="Year", y="Number of game", title = "Age target of game",color = "Age", size = "Number")+
  guides(col=guide_legend(ncol = 2))

# Question 5: Who is the top 10 designer?

games %>% 
  group_by(designer) %>% 
  mutate(aver_rating_designer = mean(average_rating)) %>% 
  ungroup() %>% 
  count(designer, aver_rating_designer) %>% 
  top_n(n=10, wt=aver_rating_designer) %>% 
  mutate(designer = fct_reorder(designer, aver_rating_designer),
         n = as.factor(n)) %>% 
  ggplot(aes(x=aver_rating_designer, y = designer, size = n))+
  geom_point()+
  theme_classic()+
  labs(x="Average", y="Designer", title = "The best 10 game designer in the world")

#### Way 2

games %>% 
  filter(designer!="NA" & designer!="(Uncredited)") %>% 
  group_by(designer) %>% 
  mutate(aver_rating_designer = mean(average_rating)) %>% 
  ungroup() %>% 
  count(designer, aver_rating_designer) %>% 
  top_n(n=10, wt=n) %>% 
  mutate(designer = fct_reorder(designer, aver_rating_designer),
         n = as.factor(n)) %>% 
  ggplot(aes(x=aver_rating_designer, y = designer, size = n))+
  geom_point()+
  theme_classic()+
  labs(x="Average", y="Designer", title = "The busiest 10 game designer in the world")


# Question 6:

g <- games %>% 
  separate_rows(mechanic, sep = ",") %>% 
  drop_na()

