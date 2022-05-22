library(tidyverse)
library(ggplot2)
games <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")
View(games)
sports_by_area <- games %>%
  filter(Genre == 'Sports')
Sales <- apply(sports_by_area[,c(6,7,8,9)], 2, sum)
data <- data.frame(Sales, area = c('North America', 'Europe', 'Japan', 'Rest of World'))
chart_1 <- ggplot(data, aes(x= "Sports", y = Sales, fill = area)) + 
  geom_bar(stat = 'identity') + 
  coord_polar(theta = "y") +
  labs(title = "Sales Percentage of Sports Genres of Each Region", 
       fill = "Region",
       x = ""
       )
