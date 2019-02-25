#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)

# select.df <- tibble(goal = c("goal 1"), 
#                     task = c("task 1"), 
#                     index = c(1), 
#                     action = c(""),
#                     progress = c(""),
#                     estimated_time = c(1),
#                     deadline = c(1))

progress.list <- c("Ongoing", "On Hold", "Complete") 

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "goal", label = "Goal", choices = select.df$goal, options=list(create=TRUE)),
        selectizeInput(inputId = "task", label = "Task", choices = select.df$task, options=list(create=TRUE)),
        selectizeInput(inputId = "progress", label = "Progress", choices = progress.list), # observeEvent?
        dateInput(inputId = "deadline", label = "Deadline", value = NULL),
        sliderInput(inputId = "estimate_time", label = "Estimated Time", min = 0, max = 100, value = 8, step = 0.5),
        #textInput(inputId = "estimate_time", label = "Estimated Time", value = NULL),
        hr(),
        actionButton("start", "Start"),
        actionButton("stop", "Stopp")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        textOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  session$onSessionEnded(function() {
    #q()
    stopApp()
  })
  
  inner.text <- as.character()
   
   output$distPlot <- renderText({
     
       
     paste0("this is what you chose: ", input$task, 
            "     ", 
            select.df$action[select.df$action %>% length()])
     
      
     
   })
   
   observeEvent(input$progress, {
     select.df <<- bind_rows(select.df, tibble(goal = input$goal,
                                               task = input$task,
                                               index = Sys.time() %>%
                                                 as.numeric(),
                                               action = "",
                                               progress = input$progress,
                                               estimated_time = input$estimate_time,
                                               deadline = input$deadline %>%
                                                 as.numeric()))
   })
   
   observeEvent(input$start, {
     select.df <<- bind_rows(select.df, tibble(goal = input$goal,
                                               task = input$task,
                                               index = Sys.time() %>%
                                                 as.numeric(),
                                               action = "start",
                                               progress = input$progress,
                                               estimated_time = input$estimate_time,
                                               deadline = input$deadline %>% 
                                                 as.numeric()))
   })
   
   observeEvent(input$stop, {
     select.df <<- bind_rows(select.df, tibble(goal = input$goal, 
                                               task = input$task,
                                               index = Sys.time() %>%
                                                 as.numeric(), 
                                               action = "stop",
                                               progress = input$progress,
                                               estimated_time = input$estimate_time,
                                               deadline = input$deadline %>% 
                                                 as.numeric()))
   })
   
   #print(select.df)
   
   # observeEvent(input$selector, {
   #   # nchar check, because emptying the text field results in "" choice.
   #   if (nchar(input$selector) && !(input$selector %in% select.df$task)) {
   #     select.df <- bind_rows(select.df, tibble(task = input$selector,
   #                                                   index = Sys.time() %>%
   #                                                     as.numeric()))
   #   }
   #   
   #   print(select.df$task)
   # })
   
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

