library(shiny)
startData <- c("Berlin", "London", "Paris")

ui <- fluidPage(
  selectizeInput("town", "Town", choices = c(startData, "new town")),
  uiOutput("newTown")
)

server <- function(input, output, session) {

  rV <- reactiveValues(towns = startData)

   output$newTown <- renderUI({
     if (input$town == "new town") {
       tagList(
         textInput("text", "New Town"),
         actionButton("entry", "save town")
       )
     }
   })
   # update selectizeInput when actionButton is clicked
   observeEvent(input$entry, {
     rV$towns <- c(rV$towns, input$text)
     updateSelectizeInput(
       session, "town", "Town",
       choices = c(rV$towns, "new town"),
       selected = input$text
     )
   })
}

# Run the application
shinyApp(ui = ui, server = server)

