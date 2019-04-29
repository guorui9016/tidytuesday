library(tidyverse)

#load all data

brexit <- read_csv("tidytuesday_2019_04_16/brexit.csv")
corbyn <- read_csv("tidytuesday_2019_04_16/corbyn.csv")
dogs <- read_csv("tidytuesday_2019_04_16/dogs.csv")
eubalance <- read_csv("tidytuesday_2019_04_16/eu_balance.csv")
pensions <- read_csv("tidytuesday_2019_04_16/pensions.csv")
trade <- read_csv("tidytuesday_2019_04_16/trade.csv")
women_research <- read_csv("tidytuesday_2019_04_16/women_research.csv")

# Question 1: Guess how britain think about exit EU?

brexit_long <- gather(brexit, key = "br_choose",value = "percent", -date, factor_key = TRUE)

q1_plot_1 <- ggplot(brexit_long, aes(x= as.Date(date, "%d/%m/%y"),y = percent, color= br_choose))+
  geom_line()+
  labs(title = "Should Britain exit from EU?", x= "Date", y = "Percent", color = "Responding")+
  scale_color_manual(labels=c("Right","Wrong"), values = c("green", "red"))+
  geom_smooth(method = lm, se = F,size = 4)+
  theme_bw()

q1_plot_1

# Question 2: Devide time to 4 stage, what is the choose in last day of each stage.


