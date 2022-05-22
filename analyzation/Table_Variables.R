library(tidyverse)


game <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")


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


#The table is grouped by the Genre of the games, 
#and find out which game in their genre have the highest sale record, who are their publishers. 
#We also include the user rate to see if sales will affect the user rate.
table <- game %>% group_by(Genre) %>% na.omit() %>% summarize(Name = Name[max_sales = max(Global_Sales)], 
                                                              max_sales = max(Global_Sales), 
                                                              User_Rate = max(User_Score), 
                                                              Publisher = Publisher[User_Rate = max(User_Score)])