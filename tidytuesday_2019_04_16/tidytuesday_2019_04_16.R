library(tidyverse)
library(mapdata)

se#load all data

brexit <- read_csv("tidytuesday_2019_04_16/data/brexit.csv")
corbyn <- read_csv("tidytuesday_2019_04_16/data/corbyn.csv")
dogs <- read_csv("tidytuesday_2019_04_16/data/dogs.csv")
eubalance <- read_csv("tidytuesday_2019_04_16/data/eu_balance.csv")
pensions <- read_csv("tidytuesday_2019_04_16/data/pensions.csv")
trade <- read_csv("tidytuesday_2019_04_16/data/trade.csv")
women_research <- read_csv("tidytuesday_2019_04_16/data/women_research.csv")

# Question 1: In data brexit.csv, guess how britain think about exit EU?

brexit_long <- gather(brexit, key = "br_choose",value = "percent", -date, factor_key = TRUE)

q1_1 <- ggplot(brexit_long, aes(x= as.Date(date, "%d/%m/%y"),y = percent, color= br_choose))+
  geom_line()+
  labs(title = "Should Britain exit from EU?", x= "Date", y = "Percent", color = "Responding")+
  scale_color_manual(labels=c("Right","Wrong"), values = c("green", "red"))+
  geom_smooth(method = lm, se = F,size = 4)+
  theme_bw()

q1_1

# Question 2: In data brexit.csv, devide time to 4 stage, what is the choose in last day of each stage.

rows <- nrow(brexit)

brexit_stage <- brexit[c(1, rows%/%3, rows%/%3*2, rows), ]

brexit_stage <- brexit_stage %>% 
  mutate(other = 100-percent_responding_right-percent_responding_wrong,
         date = as.factor(date))

brexit_stage_long <-gather(brexit_stage, key="type", value = "percent", -date) 

q2_1 <- ggplot(brexit_stage_long, aes(x= "content",y = percent, fill = type))+
  geom_bar(stat = "identity", position = "stack", width = 1)+
  facet_grid(~as.factor(as.Date(date, "%d/%m/%y")))+
  coord_polar(theta = "y")

q2_1

#### To make it looks better....

q2_2 <- ggplot(brexit_stage_long, aes(x = type, y = percent, fill = type)) +
  geom_bar(stat = "identity", alpha= 0.7)+
  coord_polar()+
  facet_grid(~as.factor(as.Date(date, "%d/%m/%y")))+
  labs(x = "", y = "")+ 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "none")

q2_2  


q2_3 <- ggplot(brexit_stage_long, aes(x = type, y = percent, fill = type)) +
  geom_bar(stat = "identity", alpha= 0.7)+
  coord_polar(theta = "y")+
  facet_grid(~as.factor(as.Date(date, "%d/%m/%y")))+
  labs(x = "", y = "")+ 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "none")

q2_3 

# Question 3: In data corbyn.csv, which political or group is the popular on Facebook?

q3_1 <- ggplot(corbyn, aes(x=political_group, y = avg_facebook_likes, fill = political_group))+
  geom_bar(stat = "identity")+
  geom_hline(aes(yintercept = mean(avg_facebook_likes)))+
  theme(axis.text.x = element_text(angle = 90))

q3_1


# Question 4: In Data dog.csv, does people like small dog more than big dog?

q4_1 <- ggplot(dogs, aes(x=year, y = avg_weight))+
  geom_path()
    
q4_1

# Question 5: Which country balance become to better?

#### Draw a graph for only one country first.

Spain <- subset(eubalance, country == "Spain")

Spain$pos <- Spain$value >= 0

q5_1 <- ggplot(Spain, aes(x = year, y = value,fill = account_type, ))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Spin balance", subtitle = "2009-2015", x="year", y="balance", fill = "type")
  
q5_1

# Question 6: Which country you want live when you old?

pensions$rate <- pensions$gov_spend_percent_gdp/pensions$pop_65_percent

q6_1 <- ggplot(pensions, aes(x = round(pop_65_percent) , y =country ))+
  geom_tile(aes(fill=rate))
q6_1

#### Work with map.

# Question 7: Which field is women work with?

q7_1 <- ggplot(women_research, aes(x = field, y = percent_women))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))
  
q7_1



#### To make it better...

q7_2 <- ggplot(women_research, aes(x = field, y = percent_women, color = field))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_color_manual(values = c("black", "black", "red", "black","green"))  
  #scale_color_manual(values = c("black", "black", "red", "black","black"))  
    
q7_2


# Question 8: Which country is lost of women go to work?

#### In this case I learn how to mean by row in 

women_research_wide <- spread(women_research, field, percent_women) 

women_research_wide$avg <- rowMeans(women_research_wide[, -1])

q7_3 <- ggplot(women_research_wide, aes(x = country, y = avg, size = avg, color = country))+
  geom_point()
q7_3

#### In this case learn how to draw data in real map.

world <- map_data("world")

ww <- left_join(world, women_research_wide, by = c("region" = "country"))

q7_4 <- ggplot(ww, aes(x=long, y = lat, group = group))+
  geom_polygon(aes(fill = avg))+
  scale_fill_viridis_c()

q7_4

            
rm(list = c("q7_1", "q7_2","q7_3","q7_4"))  

# Question 9:
