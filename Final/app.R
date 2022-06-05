library("shiny")
library("dplyr")
library("ggplot2")
library("plotly")
library("bslib")
library("ECharts2Shiny")
library("rsconnect")

# Load data frame
data <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")
video_game_df <- data
sports_by_area <- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv") %>%
  filter(Genre == 'Sports')
value <- apply(sports_by_area[,c(6,7,8,9)], 2, sum)
data111 <- data.frame(value, name = c('North America', 'Europe', 'Japan', 'Rest of World'))
table_df <- sports_by_area %>% arrange(desc(Global_Sales)) %>% slice_head(n = 10)

# intro page 
Intro_Page <- tabPanel( "Introduction", titlePanel("Introduction"),
                        sidebarLayout(
                          sidebarPanel(
                            p("
We are all video game lovers. Among all, sports are such a fun genre that we can compete against one another in tactical challenges that test our precision, accuracy, and strategy. We enjoy the competitive nature of sports games. Therefore, for this project, we focus our analysis on sports game sales. And we use the dataset that is found on Kaggle that records different platforms, countries, and years of selling data of different games from the year 2007 to 2016. This also included our requesting data: global sports games sales by platform, year of release, and different countries. 
", style = "font-family: 'times'; font-si16pt")
                          ),
                          mainPanel(
                            img(src = "ok.jpg", height = 300, width = 500),
                            p("And here are Our major concerns in this project:
", style = "font-family: 'times'; font-si16pt"),
                            h6(strong("-- Sports game released in which year sells the most?
"), style = "font-family: 'times'; font-si16pt"),
                            h6(strong("-- Which platform has the most sales on the sports games?"), style = "font-family: 'times'; font-si16pt"),
                            h6(strong("-- Which region has the largest market share?"), style = "font-family: 'times'; font-si16pt"),
                          )
                        )
)


# summary page 
Summ_Page <- tabPanel( "Summary", titlePanel("Summary"),
                       sidebarLayout(
                         sidebarPanel(
                           img(src = "vs.jpg", height = 300, width = 400)),
                         mainPanel(
                           p("Our group project used a dataset that records relevant data about games' sales between 2007 - 2016. By using this dataset, we created three charts in our shiny app. Then, we deploy these three charts on three different pages in Shiny. 
", style = "font-family: 'times'; font-si16pt"),
                           p("The first chart we create displays the sales of global sports games in different released years between 2007 and 2016. By adjusting the year controls, we minimize the range and spectate exactly how many sports games sales are there in that specific range of releasing years globally. Surprisingly, we found out that sales reached their peak in the sports games released in 2009.",
                             style = "font-family: 'times'; font-si16pt"),
                           p("Then on the second page, we have our chart records the total global sales of sports games on different sales platforms. By selecting the platforms on the menu, we can check and compare the sales of sports games published on these chosen platforms. As we expected, with 288.61 million total sales, Wii is the lead in the sports games selling platforms. PS2 is the second-highest selling platform, with 273.41 million overall sales.",
                             style = "font-family: 'times'; font-si16pt"),
                           p("Finally, we analyzed the sales percentage of sports genres of each region labeled in the dataset. We discovered that North America has the largest market share, followed by Europe. The sales of the top ten sports games in different regions of the world can also be found in our analysis. With 41.36 million sales in North America, 28.96 million sales in Europe, 3.77 million sales in Japan, and 8.45 million sales in the rest of the world, Wii Sports is the most popular sports game in the world.",
                             style = "font-family: 'times'; font-si16pt"),
                           
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
  "Year of Release",
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
  "Platforms",
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

# chart 3 page 
chart_3_page <- tabPanel(
  "Regions",
  titlePanel("Sports Genre Sales by Regions"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "select", 
                  label = h4("Select one of the most popular video game from the TOP 10 game list"), 
                  choices = table_df$Name, 
                  selected = 1)
    ),
    mainPanel(
      loadEChartsLibrary(),
      tags$div(id = "piechart", style = "width:80%; height:400px;"),
      deliverChart(div_id = "piechart"),
      tableOutput(outputId = 'table1')
    )
  )
)

ui <- navbarPage(
  # add a theme 
  theme = bs_theme(bootswatch = "minty",
                   bg = "black", fg = "white", primary = "#3f00ff",
                   base_font = font_google("Space Mono"),
                   code_font = font_google("Space Mono")),
  "Video Games Sales",
  Intro_Page,
  chart_1_page,
  chart_2_page,
  chart_3_page,
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
  
  # chart 3 
  renderPieChart(div_id = "piechart", data = data111)
  output$table1 <- renderTable({
    return(table_df %>% filter(Name == input$select) %>% select(
      Name, Platform, Year_of_Release, NA_Sales, EU_Sales, JP_Sales, Other_Sales
    ) %>% rename("Year of Release" = Year_of_Release,
                 "North America Sales (in millions)" = NA_Sales,
                 "Europe Sales (in millions)" = EU_Sales,
                 "Japan Sales (in millions)" = JP_Sales,
                 "Other Sales (in millions)" = Other_Sales)
    )
  })
}

# this is the function that makes the shiny app

shinyApp(ui = ui, server = server)