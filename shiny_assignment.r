library(tidyverse)
library(shiny)
library(readxl)
library(ggplot2)

girl_names_df <- readxl::read_excel("Top100_Popular_Baby_Names.xlsx", 
                                    sheet = 1, skip=7, n_max = 100,
                                    col_names = FALSE) 
emptycols <- colSums(is.na(girl_names_df)) == nrow(girl_names_df)
girl_names_df <- girl_names_df[!emptycols]
yr_name <- paste(1954:2018, "_name", sep = "")
yr_num  <- paste(1954:2018, "_num", sep = "")
yr_vals <- sort(c(yr_name,yr_num))
col_names <- append("rank", yr_vals)

colnames(girl_names_df) <- col_names
View(girl_names_df)
boy_names_df <- readxl::read_excel("Top100_Popular_Baby_Names.xlsx", 
                                    sheet = 2, skip=7, n_max = 100,
                                    col_names = FALSE) 
emptycols <- colSums(is.na(boy_names_df)) == nrow(boy_names_df)
boy_names_df <- boy_names_df[!emptycols]
colnames(boy_names_df) <- col_names

ui <- fluidPage(
  # 
  #   # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(inputId = "yr",
                  label = "Year:",
                  choices = paste(1954:2018, "", sep=""),
                  selected = 1954),
      selectInput(inputId = "name",
                  label = "Baby Girl Name:",
                  choices = c(girl_names_df$`1954_name`),
                  selected = NULL),
      selectInput(inputId = "name",
                  label = "Baby Boy Name:",
                  choices = c(boy_names_df$`1954_name`),
                  selected = NULL),
      # Set alpha level
      sliderInput(inputId = "alpha",
                  label = "Alpha:",
                  min = 0, max = 1,
                  value = 0.5)
    ),
    
    # Show top 10 names
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Top 10 baby girl names", tableOutput("table1")),
                  tabPanel("Top 10 baby boy names", tableOutput("table2")),
                  tabPanel("Popularity of girl names Chart", plotOutput("plot1")),
                  tabPanel("Popularity of boy names chart", plotOutput("plot2"))
          )
       )
)
)
server <- function(input, output) {
  
  # Show table with top 10 baby girl names 
  output$table1 <- renderTable({
    girl_names_df %>%
      filter(rank<=10) %>%
      select(contains(input$yr)) %>%
      select(contains("name"))
    })
  
  # Show table with top 10 baby boy names 
  output$table2 <- renderTable({
    boy_names_df %>%
      filter(rank<=10) %>%
      select(contains(input$yr)) %>%
      select(contains("name"))
  })
  
  plotData1 <- reactive(
  #   filter(girl_names_df, input$name %in% 'name')
    girl_names_df  %>%
    select(contains(input$name)
  ))
  # plotData2 <- reactive( 
  # yvar <- girl_names_df$rank
  # )
  # plotData3()
  # Create scatterplot object the plotOutput function is expecting
  output$plot1 <- renderPlot({
     ggplot(data = girl_names_df, aes(x = input$yr, y = girl_names_df$rank ) +
     geom_point(alpha = input$alpha))
  })
}



shinyApp(ui = ui, server = server)