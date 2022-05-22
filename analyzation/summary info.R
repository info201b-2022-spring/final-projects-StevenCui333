
game <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")


summary_info <- list()
summary_info$num_of_observations <- nrow(game)
summary_info$globalSaleTotal <- sum(na.omit(game$Global_Sales))
summary_info$NASaleTotal <-  sum(game$NA_Sales)
summary_info$JapanSaleTotal <- sum(game$JP_Sales)
summary_info$Amount_of_Nietendo_Published_games <- sum((game$Publisher == "Nintendo"))

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
