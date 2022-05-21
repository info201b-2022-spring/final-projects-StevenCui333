library(tidyverse)


game <- read.csv("video.csv")
summary_info <- list()


JapanSaleTotal <- sum(game$JP_Sales)

NASaleTotal <-  sum(game$NA_Sales)

globalSaleTotal <- sum(na.omit(game$Global_Sales))
                   
Amount_of_Nietendo_Published_games <- sum((game$Publisher == "Nintendo"))

published_games_in_total <- nrow(game)
  
Most_publish_Genre_of_Game <- game %>% group_by(game$Year_of_Release, game$Genre) %>% na.omit(summarise((max(sum(game$JP_Sales))), count = count(game$Name)))
  
table <- game %>% group_by(Genre) %>% filter(Global_Sales == min(Global_Sales)) 



