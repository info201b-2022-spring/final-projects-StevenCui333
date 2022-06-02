library(shiny)
library(tidyverse)

# load in data
data <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")

# How many years do you want to view

# set up the UI
ui <- fluidPage(
 titlePanel("sales by year release"),
 sidebarLayout(
   sidebarPanel(
     h5("Controls"),
     sliderInput(
       inputId = "yearInput", 
       label = "Year",
       min = min(Sports_df$Year_of_Release),
       max = max(Sports_df$Year_of_Release),
       value = c(min(Sports_df$Year_of_Release),max(Sports_df$Year_of_Release)))
   ),
   mainPanel(
     plotOutput(outputId = "scatter", brush = "plot_brush"),
     tableOutput(outputId = "data"),
     plotlyOutput(outputId = "plotlyScatter"),
     textOutput(outputId = "myText")
   )
 )
)


# Server logic goes here

server <- function(input, output) {
  output$scatter <- renderPlot({
    data <- data %>%na.omit(data)%>%
      arrange(Year_of_Release)%>%
      filter(Genre == "Sports")%>%
      filter(Year_of_Release != "N/A")
    
    Sports_df <-  data %>% 
      group_by(Year_of_Release)%>%
      summarise(Sports_sales = sum(Global_Sales))
    
    Sports_df[] <- lapply(Sports_df, as.numeric)
    
      Sports_df%>%
      tail(10) %>%
      ggplot( aes(x=Year_of_Release, y=.data[[input$yearInput]])) +
      geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
      ggtitle("Global Sports Games Sales by different year of release ") + labs(y = "Sports_sales(in millions)")
  })
      output$data <- renderTable({
        brushedPoints(Sports_df, input$plot_brush)
      })
      
      output$plotlyScatter <- renderPlotly({
        plot_ly(data = Sports_df, x = ~Year_of_Release, y = ~Sports_sales, color = ~Sports_sales, type = "scatter")
      })
      
      output$myText <- renderText({
        paste("When you select a radioButton, this is what the server interpets it as:", input$yearInput) 
      })
      
    
}



# this is the function that makes the shiny app

shinyApp(ui = ui, server = server)