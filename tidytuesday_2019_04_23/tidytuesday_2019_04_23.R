library(tidyverse)
library(lubridate)

anime <- read_csv("tidytuesday_2019_04_23/data/tidy_anime.csv")

# Question 1: Do anime become to better and better?

#### Why anime_unique is

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

rm(list= "q1_1")

# Question 2: Quanitity or Quality?

q2_1 <- anime_unique %>%
  filter(!is.na(studio)) %>% 
  group_by(studio) %>% 
  summarise(n = n(),
            rank_avg = mean(rank)) %>% 
  ggplot(aes(x=rank_avg, y = n))+
  geom_point(aes(size = rank_avg, color = rank_avg), show.legend = F)+
  labs(x = "Average of Rank(smaller is better)", y = "Number of anime from studio", title = "Quanitity or Quality?")+
  theme(plot.background = element_rect("#fceec7"),
        plot.title = element_text(size = 25, hjust = 0.4,face = "bold"), 
        panel.background = element_rect("#fceec7"))+
  scale_color_continuous(low = "green", high = "blue")

q2_1

rm (list  ="q2_1")

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
rm(list = "q3_1")

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

q4 <- anime_unique %>% 
  left_join(studio_rank, by= "studio") %>% 
  filter(studio_rank < 11) %>% 
  ggplot(aes(x = reorder(studio,rank_avg), y = rank))

#### Q4 Way 1

q4 +  geom_violin()+
  geom_point(color = "gray", alpha = 0.5)

#### Q4 way 2
q4 + geom_boxplot()

rm(list = "q4")

# Question 5: Number of anime in different genre

q5 <- anime %>% 
  mutate(year = year(start_date)) %>%
  filter(!is.na(genre), year > 1960) %>% 
  group_by(genre, year) %>% 
  summarise(n = n()) %>% 
  ggplot()

#### Q5 way 1

#### I got a warning message.

q5 +  geom_tile(aes(x = year, y = genre,fill=n))+
  labs(x = "Year", y = "Genre", title = "Number of anime in different genre")

#### Q5 way 2

q5 + geom_area(aes(x = year, y = n, fill=genre))+
  labs(x = "Year", y = "Number of anime", title = "Number of anime in different genre")

rm(lsit="q5")

# Question 6: Which way people prefer to watch anime?

Q6 <- anime_unique %>% 
  select(animeID, name, type, members) %>% 
  group_by(type) %>% 
  summarise(sum_member = sum(members)) %>% 
  ggplot(aes(x= sum_member, y = reorder(type, sum_member)))+
  theme_bw()

Q6 + geom_point(color = "darkblue", size = 5, alpha=0.5)+
  geom_segment(aes(yend = type), xend = 0, color = "darkblue",size  =2,alpha=0.2)

rm(list = "Q6")

# Question 7: Dose people like original anime more than menga in top 20?

#### how to get top 20 from each type??

Q7 <- anime_unique %>% 
  arrange(desc(score)) %>% 
  head(20) %>% 


