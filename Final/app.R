library("shiny")
library("dplyr")
library("ggplot2")
library("plotly")
library("bslib")

# Load data frame
data <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")
video_game_df <- data

# add a theme

# About page
about <- tabPanel(
  "About"
)


# intro page
Intro_Page <- tabPanel( "Introduction",
                         titlePanel("Introduction"),
                         sidebarLayout(
                           sidebarPanel(
                             p("
We are all video game lovers. Among all, sports are such a fun genre that we can compete against one another in tactical challenges that test our precision, accuracy, and strategy. We enjoy the competitive nature of sports games. Therefore, for this project, we focus our analysis on sports games sales.
We found this dataset that includes all the variables related to our interest: global sports games sales by platform, year of release, and different countries.
", style = "font-family: 'times'; font-si10pt")
                           ),
                           mainPanel(
                             img(src = "yes.jpg", height = 140, width = 400)
                           )
                         )
)


#Summary
Summ_Page <- tabPanel(  "Summary", titlePanel("Summary"),
                         sidebarLayout(
                           sidebarPanel(
                             p("
In our group project, we used a dataset that records  relevant data about games¡¯ sales between 1980 - 2016. And by using this dataset, we created three charts, find 5 variable and a table that includes data that we are interested in.
The first chart we create displays the sales percentage of sports genres of each region labeled in the dataset.And regions are differentiated by different colors. The second chart is a column type chart which shows the sale data of sport games in different platform. For the last chart, it explores the relationship between the global Sports Game sales on December 22, 2016 and the year of release. 
Other than these three charts, we calculated five variables: JapanSaleTotal, NASaleTotal, globalSaleTotal, Amount_of_Nietendo_Published_games, published_games_in_total. And they respectively represent, the total amount of game sales in Japan, the total amount of game sale in North America, the total amount of game sales globally, the amount the of the games that Nietendo has published, and total published games. (All these variables, can only represent results between 1980 to 2016)
Beside what¡¯s described above, the table we create has 5 column in total which respectively represent what we are interested in about the dataset. In the table we record the Genere of the games, and find out which game in their genre have the highest sale record, who are their publishers. We also include the user rate to see if sales will affect the user rate.

", style = "font-family: 'times'; font-si16pt")
                           ),
                           mainPanel(
                             img(src = "yes.jpg", height = 140, width = 400)
                           )
                         )
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
  titlePanel("Global Sports Games Sales by Different Year of Release"),
  sidebarLayout(
    sidebarPanel(
      h4("Controls"),
      numericInput(inputId = "num", label = h3("Started Year"), value = 2007, min = 2007),
      numericInput(inputId = "num2", label = h3("End Year"), value = 2016, max = 2016)
    ),
    mainPanel(
      plotOutput(outputId = "plot", brush = "plot_brush"),
      textOutput(outputId = "text"),
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
  theme = bs_theme(bootswatch = "minty",
                   bg = "#e7feff", fg = "black", primary = "#3f00ff",
                   base_font = font_google("Space Mono"),
                   code_font = font_google("Space Mono")),
  "Video Games Sales",
  Intro_Page,
  chart_1_page,
  chart_2_page,
  Summ_Page
)


server <- function(input, output) {
  # chart 1 
  output$plot <- renderPlot({
    Sports_df%>%
      tail(10) %>%
      ggplot( aes(x=Year_of_Release, y=Sports_sales)) +
      geom_point(shape=21, color="black", fill="#3f00ff", size=6) +
      xlim(input$num, input$num2) + 
      labs(x = "Year of Release",
           y = "Sports Sales (in millions)")
    
  })
  
  output$text <- renderText({
    paste("Note: Brush the plot to see the value of the circled points in the table below")
  })
    
  output$data <- renderTable({
    brushedPoints(Sports_df, input$plot_brush)%>%
      rename("Sports Sales (in millions)" = Sports_sales) %>%
      rename("Year of Release" = Year_of_Release)
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
            marker = list(color = "#3f00ff"),
            type = "bar"
    ) %>% layout (yaxis = list(title = 'Total Sales (in millions)'))
  })
}

# this is the function that makes the shiny app

shinyApp(ui = ui, server = server)