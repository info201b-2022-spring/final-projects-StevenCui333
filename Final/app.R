library("shiny")
library("dplyr")
library("ggplot2")
library("plotly")


# Load data frame
data <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")
video_game_df <- data
# Get continent names
#continents <- data.frame(unique(covid_data$continent[covid_data$continent != ""]))

# Get country names
#countries <- data.frame(unique(covid_data$iso_code[covid_data$continent != ""]))

# About page
about <- tabPanel(
  "About"
)

# Chart 1 page
## wrangling data
data <- data %>%na.omit(data)%>%
  arrange(Year_of_Release)%>%
  filter(Genre == "Sports")%>%
  filter(Year_of_Release != "N/A")

Sports_df <- data %>%
  group_by(Year_of_Release)%>%
  summarise(Sports_sales = sum(Global_Sales))
Sports_df[] <- lapply(Sports_df, as.numeric)
## end of wrangling data

chart_1_page <- tabPanel(
  "First Page",
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

# Chart 2 page
sports_df <- video_game_df %>% 
  filter(Genre == "Sports") %>% 
  group_by(Platform) %>% 
  summarize(sum_total = sum(Global_Sales))

chart_2_page <- tabPanel(
  "Second Page",
  titlePanel("Total Global Sales of Sports Game in Different Sales Platforms"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput( inputId = "checkGroup", 
                          label = h4("Select displayed platforms"), 
                          choices = sports_df$Platform,
                          selected = c("Wii", "X360", "PS4")
      )
      
    ),
    mainPanel(
      plotlyOutput(outputId = 'col'),
      tableOutput(outputId = 'table')
    )
  )
)

ui <- navbarPage(
  "Covid Cases and Vaccinations",
  chart_1_page,
  chart_2_page
)


server <- function(input, output) {
  # chart 1 
  output$plot <- renderPlot({
    Sports_df%>%
      tail(10) %>%
      ggplot( aes(x=Year_of_Release, y=Sports_sales)) +
      geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
      xlim(input$num, input$num2)+
      ggtitle("Global Sports Games Sales by different year of release ") + labs(y = "Sports_sales(in millions)")
    
  })
  
  output$data <- renderTable({
    brushedPoints(Sports_df, input$plot_brush)%>%
      rename("Sports sales (in millions)" = Sports_sales)
  })
  
  # chart 2 
  output$table <- renderTable({
    #return(filter(char_df, Character == input$char))
    nearPoints(sports_df,input$plot_hover, xvar = "Platform",
               yvar = "sum_total")
    return(sports_df %>% 
             filter(Platform %in% c(input$checkGroup)) %>% 
             rename("Total Sales (in millions)" = sum_total))
  })
  output$col <- renderPlotly({
    filtered_sports_df <- sports_df %>% 
      filter(Platform %in% c(input$checkGroup))
    plot_ly(data = filtered_sports_df, 
            x = ~Platform, y = ~sum_total, 
            type = "bar"
    ) %>% layout (yaxis = list(title = 'Total Sales (in millions)'))
  })
}

# this is the function that makes the shiny app

shinyApp(ui = ui, server = server)