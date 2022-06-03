library(shiny)
library(tidyverse)
library(plotly)
# load in data
data <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")

# How many years do you want to view

## wrangling data
data <- data %>%na.omit(data)%>%
  arrange(Year_of_Release)%>%
  filter(Genre == "Sports")%>%
  filter(Year_of_Release != "N/A")
Sports_df <-  data %>%
  group_by(Year_of_Release)%>%
  summarise(Sports_sales = sum(Global_Sales))
Sports_df[] <- lapply(Sports_df, as.numeric)
## end of wrangling data


# set up the UI
ui <- fluidPage(
  titlePanel("sales by year release"),
  sidebarLayout(
    sidebarPanel(
      h5("Controls"),
      numericInput(inputId = "num", label = h3("Numeric input"), value = 1998, min = 1998),
      numericInput(inputId = "num2", label = h3("Numeric input"), value = 2016, max = 2016)
    ),
    mainPanel(
      plotOutput(outputId = "plot", brush = "plot_brush"),
      tableOutput(outputId = "data"),
    )
  )
)

# Server logic goes here

server <- function(input, output) {
  output$plot <- renderPlot({
    Sports_df%>%
      tail(10) %>%
      ggplot( aes(x=Year_of_Release, y=Sports_sales)) +
      geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
      xlim(input$num, input$num2)+
      ggtitle("Global Sports Games Sales by different year of release ") + labs(y = "Sports_sales(in millions)")
    
  })
  
  output$data <- renderTable({
    brushedPoints(Sports_df, input$plot_brush)
  })
  
}

# this is the function that makes the shiny app

shinyApp(ui = ui, server = server)