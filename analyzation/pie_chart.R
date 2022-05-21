library(tidyverse)
library(ggplot2)
games <- read.csv("Video_Game_Sales_as_at_22_Dec_2016")
sports_by_area <- games %>%
  filter(Genre == 'Sports')
Sales = apply(sports_by_area[,c(5,6,7,8)], 2, sum)
data <- data.frame(Sales, area = c('North America', 'Europe', 'Japan', 'Rest of World'))

ggplot(data, aes(x = "Sports", y = Sales, fill = area)) + geom_bar(stat = 'identity') + coord_polar(theta = "y")
