library(tidyverse)
library(lubridate)

anime <- read_csv("tidytuesday_2019_04_23/data/tidy_anime.csv")

# Question 1: Do anime become to better and better?

anime_unique <- anime %>% 
  distinct(animeID, .keep_all = T) %>% 
  filter(scored_by > 1000) %>% 
  mutate(decade = as.character((year(start_date)%/%10) * 10))

q1_1 <- ggplot(anime_unique,aes(x = decade, y = score))+
  geom_jitter(aes(size = scored_by, color = decade, alpha = 0.1), show.legend = F)+
  scale_color_viridis_d()+
  labs(x = "Decade", y = "Score", title = " Does anime become to better?")+
  theme(plot.background = element_rect("black"),
        panel.background =element_rect("black"),
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.47, vjust = 1,color = "white", size = 20 ),
        axis.title = element_text(color = "white"),
        axis.text = element_text(color = "white"), 
        )
  
q1_1

# Question 2: Quanitity or Quality?

q2_1 <- anime_unique %>%
  filter(!is.na(studio)) %>% 
  group_by(studio) %>% 
  summarise(n = n(),
            rank_avg = mean(rank)) %>% 
  ggplot(aes(x=rank_avg, y = n))+
  geom_point(aes(size = rank_avg, color = rank_avg), show.legend = F)+
  labs(x = "Average of Rank", y = "Number of anime from studio", title = "Quanitity or Quality?")+
  theme(plot.background = element_rect("#fceec7"),
        plot.title = element_text(size = 25, hjust = 0.4,face = "bold"), 
        panel.background = element_rect("#fceec7"))+
  scale_color_continuous(low = "green", high = "blue")

q2_1

# Question 3: Do People change taste? 

q3_1 <- anime %>% 
  mutate(year = year(start_date)) %>% 
  group_by(genre, year) %>% 
  mutate(genre_fav = mean(favorites)) %>% 
  filter(!is.na(genre), year > 1970) %>% 
  ggplot(aes(x= year, y = reorder(genre, genre_fav)))+
  geom_tile(aes(fill = genre_fav))+
  labs(x = "Year", y = "Genre", title = "Do People change taste?", fill = "Favorite")+
  theme_classic()

q3_1

# Question 4: Which studio is the best using game as source?

#### How to get the last 10 small number in one colnum but multi-row？ 

studio_rank <-  anime_unique %>% 
  filter(source == "Game", !is.na(studio)) %>% 
  group_by(studio) %>% 
  mutate(rank_avg = mean(rank)) %>%
  distinct(studio, .keep_all = T) %>% 
  select(studio,rank_avg) %>%
  ungroup() %>% 
  arrange(rank_avg) %>%
  mutate(studio_rank = c(1:length(unique(studio))))

q4_1 <- anime_unique %>% 
  left_join(studio_rank, by= "studio") %>% 
  filter(studio_rank < 11) %>% 
  ggplot(aes(x = reorder(studio,rank_avg), y = rank))+
  geom_boxplot()
  
q4_1

rm(list = "studio_rank")

# Question 5: Number of anime in different genre

q5_1 <- anime %>% 
  mutate(year = year(start_date)) %>%
  filter(!is.na(genre)) %>% 
  group_by(genre, year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = year, y = genre))+
  geom_tile(aes(fill=n))
q5_1