library(tidyverse)
library(shiny)
library(readxl)
library(ggplot2)
library(DT)

prepare_data <- function(data){
  data[1] <- NULL
  new_data<- as.data.frame(NULL)
  new_data = data[,1:3]
  Year <- new_data[1,2]
  new_data$Year = as.numeric(Year)  
  new_data$Rank <- new_data$X__2
  new_data[1] <- NULL

  treated_data <- as.data.frame(matrix(0, ncol = 4, nrow = 0))
  colnames(new_data) <- c("Name", "No", "Year", "Rank")
  colnames(treated_data) <- c("Name", "No", "Year", "Rank")
  treated_data <- rbind(treated_data, new_data[!is.na(new_data$Rank), ])
  new_data <- as.data.frame(NULL)
  loop = TRUE
  collen = length(colnames(data))
  i <- 4
  while(loop == TRUE) {
    new_data = data[,i:(i+2)]
    Year =  new_data[1,1]
    new_data$Year = as.numeric(Year)
    new_data[1] <- NULL
    new_data$Rank <- data$X__2
    colnames(new_data) <- c("Name", "No", "Year", "Rank")
    colnames(treated_data) <- c("Name", "No", "Year", "Rank")
    treated_data <- rbind(treated_data, new_data[!is.na(new_data$Rank), ])
    new_data <- as.data.frame(NULL)
    i = i + 3
    if(i > collen){
      loop = FALSE
    }
  }
  treated_data$No <- as.integer(treated_data$No)
  return(treated_data)
}

girl_names <- read_excel("Top100_Popular_Baby_Names.xlsx", sheet = 1, col_names = FALSE, skip = 4)
girl_names_df <- prepare_data(girl_names)
girl_names_df <- na.omit(girl_names_df)

View(girl_names_df)

boy_names <- read_excel("Top100_Popular_Baby_Names.xlsx", sheet = 2, col_names = FALSE, skip = 4)
boy_names_df <- prepare_data(boy_names)
boy_names_df <- na.omit(boy_names_df)
View(boy_names_df)

ui <- fluidPage(
  # 
  #   # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(inputId = "yr",
                  label = "Year:",
                  choices = c(girl_names_df$`Year`),
                  selected = 1954),
      selectInput(inputId = "Girl_Name",
                  label = "Baby Girl Name:",
                  choices = c(girl_names_df$`Name`),
                  selected = NULL),
      selectInput(inputId = "Boy_Name",
                  label = "Baby Boy Name:",
                  choices = c(boy_names_df$`Name`),
                  selected = NULL)
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
  
  girl_chart <- reactive(
    {
      girl_popularity <- input$Girl_Name
      input_girl <- girl_names_df[girl_names_df$Name == girl_popularity,]
      return(input_girl[order(input_girl$Year),][c('Year', 'No')])
    }
  )
  boy_chart <- reactive(
    {
      boy_popularity <- input$Boy_Name
      input_boy <- boy_names_df[boy_names_df$Name == boy_popularity,]
      return(input_boy[order(input_boy$Year),][c('Year', 'No')])
    }
  )

  # Show table with top 10 baby girl names
  output$table1 <- renderTable({
    girl_names_df %>%
      filter(Rank<=10) %>%
      filter(Year==input$yr) %>%
      select(contains("Name"))
  })

  # Show table with top 10 baby boy names
  output$table2 <- renderTable({
    boy_names_df %>%
      filter(Rank<=10) %>%
      filter(Year==input$yr) %>%
      select(contains("Name"))
  })

  output$plot1 <- renderPlot({
    girl_data <- girl_chart()
    ggplot(data = girl_data, aes(x= Year, y= No)) +
      geom_point()
  })
  
  output$plot2 <- renderPlot({
    boy_data <- boy_chart()
    ggplot(data = boy_data, aes(x= Year, y= No)) +
      geom_point()
  })
}

shinyApp(ui = ui, server = server)
