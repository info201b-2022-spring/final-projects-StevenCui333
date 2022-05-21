library("tidyverse")
video_game_df <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")

sports_data <- video_game_df %>% 
  filter(Genre == "Sports") %>% 
  group_by(Platform) %>% 
  summarize(sum_total = sum(Global_Sales))

chart_2 <- sports_data %>% 
  ggplot(mapping = aes(x = Platform, y = sum_total)) + geom_col() + 
  coord_flip() +
  labs(title = "Total Global Sales of Sports Game in Different Platforms", 
       x = "Platforms", 
       y = "Total Sales (in millions)")

total_Sports_sale <- sports_data %>% summarize(sum = sum(sum_total))
# 1332
# (288.61 + 273.41)/1332 = 0.4219369

# This column chart evals the relationship between platforms and sports game sales.
# According to this chart, with 288.61 millions total sales, Wii is the highest 
# selling platform for sports games. Second only to the Wii, PS2 is the second 
# highest selling platform, with 273.41 millions overall sales. 
# 