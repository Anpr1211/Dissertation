# importing libraries

library(shiny)
library(shinydashboard)
library(tsdl)
library(forecast)

# list of subjects covered by the tsdl library

subject = sort(unique(meta_tsdl$subject))

# function to get the titles of the time series of the chosen category

get_title = function(category){
  
  sub_cat <- subset(tsdl, category)
  title_list = list()
  
  for (x in 1:length(sub_cat)){
    data <- attributes(sub_cat[[x]])
    title_list[x] <- data$description
  }
  
  return(title_list)
}

# function to get the source of the chosen time series

get_source = function(title){
  
  data <- subset(tsdl, description=title)
  source = attributes(data[[1]])$source
  return(source)
}

# function to get the start date of the chosen time series

get_startdt = function(title){
  
  data <- subset(tsdl, description=title)
  startdt = attributes(data[[1]])$tsp[1]
  return(startdt)
}

# function to get the end date of the chosen time series

get_enddt = function(title){
  
  data <- subset(tsdl, description=title)
  enddt = attributes(data[[1]])$tsp[2]
  return(enddt)
}

# function to get the frequency of the chosen time series

get_freq = function(title){
  
  data <- subset(tsdl, description=title)
  freq = attributes(data[[1]])$tsp[3]
  return(freq)
}

# function to get data of the chosen time series

get_data = function(title){
  
  data <- subset(tsdl, description=title)
  return(data[[1]])
}

# user interface of the web app

ui <-fluidPage(
  titlePanel("tsdl library - Data Visualizations"),
  fluidRow(
    column(3, wellPanel(
      # This outputs the dynamic UI component - the list of subjects
      selectInput("subject", " Subject", subject)
    )),
    column(9, wellPanel(
      # This outputs the dynamic UI component - the list of titles of the time series of the chosen subjects
      uiOutput("ui")
    )),
    # This outputs the dynamic UI component - the details of the time series
    column(3,
           tags$h2("Stuff you might want to know -")
    ),
    column(4,
           tags$p("Data Source :"),
           verbatimTextOutput("source"),
           tags$p("Frequency :"),
           verbatimTextOutput("freq")
    ),
    column(4,
           tags$p("Start :"),
           verbatimTextOutput("startdt"),
           tags$p("End :"),
           verbatimTextOutput("enddt")
           
           ),
   # This outputs the dynamic UI component - the plot of the time series
  plotOutput("plot")
)
)

# the seerver function

server <- function(input, output){
  
  output$ui <- renderUI({
  
    switch(input$subject,
           "Agriculture" = selectInput("title", "Title", get_title("Agriculture")),
           "Chemistry" = selectInput("title", "Title", get_title("Chemistry")),
           "Computing" = selectInput("title", "Title", get_title("Computing")),
           "Crime" = selectInput("title", "Title", get_title("Crime")),
           "Demography" = selectInput("title", "Title", get_title("Demography")),
           "Ecology" = selectInput("title", "Title", get_title("Ecology")),
           "Finance" = selectInput("title", "Title", get_title("Finance")),
           "Health" = selectInput("title", "Title", get_title("Health")),
           "Hydrology" = selectInput("title", "Title", get_title("Hydrology")),
           "Industry" = selectInput("title", "Title", get_title("Industry")),
           "Labour market" = selectInput("title", "Title", get_title("Labour market")),
           "Macroeconomic" = selectInput("title", "Title", get_title("Macroeconomic")),
           "Meteorology" = selectInput("title", "Title", get_title("Meteorology")),
           "Microeconomic" = selectInput("title", "Title", get_title("Microeconomic")),
           "Miscellaneous" = selectInput("title", "Title", get_title("Miscellaneous")),
           "Physics" = selectInput("title", "Title", get_title("Physics")),
           "Production" = selectInput("title", "Title", get_title("Production")),
           "Sales" = selectInput("title", "Title", get_title("Sales")),
           "Sport" = selectInput("title", "Title", get_title("Sport")),
           "Transport and tourism" = selectInput("title", "Title", get_title("Transport and tourism")),
           "Tree-rings" = selectInput("title", "Title", get_title("Tree-rings")),
           "Utilities" = selectInput("title", "Title", get_title("Utilities"))
           )
    
    
  })
  
  output$source <- renderText({get_source(input$title)})
  output$startdt <- renderText({get_startdt(input$title)})
  output$enddt <- renderText({get_enddt(input$title)})
  output$freq <- renderText({get_freq(input$title)})
  
  output$plot <- renderPlot(plot(get_data(input$title), ylab="Value"))
}

shinyApp(ui, server)
