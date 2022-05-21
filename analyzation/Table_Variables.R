library(tidyverse)


game <- read.csv("video.csv")
summary_info <- list()

 
#the total amount of game sales in Japan
JapanSaleTotal <- sum(game$JP_Sales)

#the total amount of game sale in North America
NASaleTotal <-  sum(game$NA_Sales)

#the total amount of game sales globally
globalSaleTotal <- sum(na.omit(game$Global_Sales))

#the amount the of the games that Nietendo has published                
Amount_of_Nietendo_Published_games <- sum((game$Publisher == "Nintendo"))

#and total published games
published_games_in_total <- nrow(game)
 
  
table <- game %>% group_by(Genre) %>% summarize(Name == Name[max_sales = max(Global_Sales)], 
                                                max_sales = max(Global_Sales), 
                                                User_Rate = max(User_Rate), 
                                                Publisher == Publisher[max_sales = max(Global_Sales)])



