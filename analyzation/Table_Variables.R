library(tidyverse)


game <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")


#The table is grouped by the Genre of the games, 
#and find out which game in their genre have the highest sale record, who are their publishers. 
#We also include the user rate to see if sales will affect the user rate.
table <- game %>% group_by(Genre) %>% na.omit() %>% summarize(Name = Name[max_sales = max(Global_Sales)], 
                                                              max_sales = max(Global_Sales), 
                                                              User_Rate = max(User_Score), 
                                                              Publisher = Publisher[User_Rate = max(User_Score)])
