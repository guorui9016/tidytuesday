library(tidyverse)

bike <- read_csv("bike_traffic.csv")

bike <- as.numeric(bike)

bike_count_sort <-  arrange(bike, desc(bike$bike_count))

# Do not know why it the bike_count is NA

bike_crossing <- data.frame(bike) %>% 
  group_by(direct) %>% 
  summarise(avg_bike = sum(bike_count))

