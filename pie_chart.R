library(ggplot2)
games <- read.csv("ps4_sales.csv")
sports_by_area <- games %>%
  filter(Genre == 'Sports')
Sales = apply(sports_by_area[,c(5,6,7,8)], 2, sum)
data <- data.frame(Sales, area = c('North America', 'Europe', 'Japan', 'Rest of World'))

ggplot(data, aes(x = "Sports", y = Sales, fill = area)) + geom_bar(stat = 'identity') + coord_polar(theta = "y")