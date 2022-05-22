
library(tidyverse)
data <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")

data <- data %>%na.omit(data)%>%
        arrange(Year_of_Release)%>%
        filter(Genre == "Sports")%>%
        filter(Year_of_Release != "N/A")

Sports_df <-  data %>% 
          group_by(Year_of_Release)%>%
          summarise(Sports_sales = sum(Global_Sales))
  

Sports_df[] <- lapply(Sports_df, as.numeric)
          
plot <- Sports_df%>%
        tail(10) %>%
        ggplot( aes(x=Year_of_Release, y=Sports_sales)) +
        geom_line( color="grey") +
        geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
        ggtitle("Global Sports Games Sales by Different Year of Release ") + 
  labs(y = "Sports Sales (in millions)", 
       x = "Year of Release")

