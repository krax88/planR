#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork)
library(tidyverse)
library(lubridate)

nodes <- tibble(id = as.numeric(),
                label = as.character())
edges <- tibble(from = as.numeric(), 
                to = as.numeric())

# Define UI for application that draws a histogram
ui <- navbarPage(
   
   # Application title
   "PlanR",
   tabPanel("Network"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        textInput(inputId = "task_label",
                  label = "task label",
                  value = "test"),
        selectizeInput(inputId = "task_relation_from",
                    label = "from task",
                    choices = nodes$label),
        selectizeInput(inputId = "task_relation_to",
                       label = "to task",
                       choices = nodes$label),
        actionButton(inputId = "goButton",
                     label = "Update View", 
                     icon("refresh")),
        width = 3
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        visNetworkOutput("network"),
        width = 9
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # tibble()
  # nodes <- tibble(id = as.numeric(),
  #                 label = as.character())
  # edges <- tibble(from = as.numeric(), 
  #                 to = as.numeric())
   
   output$network <- renderVisNetwork({
     
     input$goButton

     nodes <<- if((nodes$id %in% inner.tibble$id) %>% `!` %>% all()){
       bind_rows(nodes, inner.tibble)
     }else{
       nodes
     }
     
     print(nodes)
     
     assign(
       x = "nodes",
       value = nodes,
       envir = .GlobalEnv
     )
     
     print(nodes)
     
     edges <<- bind_rows(edges, inner.tibble.2)

     assign(
       x = "edges",
       value = edges,
       envir = .GlobalEnv
     )

     print(edges)
     
     visNetwork(nodes, edges) %>% 
       visEdges(arrows = "to") %>% 
       visHierarchicalLayout(direction = "RL", levelSeparation = 500)
       #visOptions(manipulation = TRUE) %>% 
       #visNodes()
       #visInteraction(navigationButtons = TRUE)
     
     
     
     # observe({
     #   visNetworkProxy("network_proxy_nodes") %>%
     #     visUpdateNodes(nodes = input$mynetwork__graphChange)
     # })
     
   })
   
   #proxy = visNetworkProxy("network")
   
   observeEvent(input$task_label, {
     inner.tibble <<- tibble(id = Sys.time() %>% 
                               as.numeric(), 
                            label = c(input$task_label))
     
     print(paste0("detta möget: ",input$task_relation_from))
     
     print("A")
     visNetworkProxy("network") %>% 
       visUpdateNodes(nodes = bind_rows(nodes, inner.tibble))
     print("b")
   })
   
   
   observeEvent(input$task_relation_from, {
     inner.tibble.2 <<- tibble(from = nodes$id[which(nodes$label %in% input$task_relation_from)], 
                               to = nodes$id[which(nodes$label %in% input$task_relation_to)])
     
     print(paste0("detta möget: ",input$task_relation_from))
     
     print("A")
     visNetworkProxy("network") %>% 
       visUpdateEdges(edges = bind_rows(edges, inner.tibble.2))
     print("b")
   })
   
   observeEvent(input$task_relation_to, {
     inner.tibble.3 <<- tibble(from = nodes$id[which(nodes$label %in% input$task_relation_from)], 
                               to = nodes$id[which(nodes$label %in% input$task_relation_to)])
     
     print(paste0("detta möget: ",input$task_relation_from))
     
     print("A")
     visNetworkProxy("network") %>% 
       visUpdateEdges(edges = bind_rows(edges, inner.tibble.3))
     print("b")
   })
   
   observe({
     
     print("c")
     input$goButton
     
     print(Sys.time())
     
     print("d")
   updateSelectizeInput(session, 
                        'task_relation_from', 
                        choices = nodes$label, 
                        server = TRUE)
   
   })
   
   observe({
     
     print("e")
     input$goButton
     
     print(Sys.time())
     
     print("f")
     updateSelectizeInput(session, 
                          'task_relation_to', 
                          choices = nodes$label, 
                          server = TRUE)
   })
   
     
   # observeEvent(input$task_label, {
   #   
   #   
   #   inner.tibble <<- tibble(from = as.numeric(), 
   #                           to = as.numeric())
   #   
   #   visNetworkProxy("network") %>% 
   #     visUpdateEdges(nodes = bind_rows(nodes, inner.tibble))
   # })
   

   
   
   
   # observeEvent(input$mynetwork__graphChange, {
   #   info = input$mynetwork__graphChange
   #   print(Sys.time())
   # })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

