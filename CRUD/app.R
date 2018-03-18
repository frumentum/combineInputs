library(shiny)
library(DT)

startData <- data.frame(
  town = c("Berlin", "London", "Paris"),
  population = c(3711930, 9787426, 12142802),
  stringsAsFactors = FALSE
)

# Define UI for application that draws a histogram
ui <- fluidPage(
   uiOutput("editDB"),
   dataTableOutput("table", width = "70%")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # make data.frame reactive
  rV <- reactiveValues(towns = startData)

  # render table output; only single selection is allowed
  output$table <- DT::renderDataTable({
    DT::datatable(rV$towns, selection = "single")
  })

  # Reactive function to determine if or which row is selected
  selection <- reactive({
    if (!is.null(input$table_rows_selected))
      return(input$table_rows_selected) else return(0)
  })

  # if a row is selected, one will get the possibility to edit the town;
  # if no row is selected (or it's deselected), edit mode shall disappear
  observeEvent(selection(), {
    output$editDB <- renderUI({
      if (selection() != 0) {
        # which town is selected? create character string
        selectedTown <- rV$towns$town[input$table_rows_selected]

        # depending on the selectizeInput, textInput occurs
        tagList(
          selectizeInput("town", "Town",
                         choices = c(unique(rV$towns$town), "new town"),
                         select = selectedTown),
          conditionalPanel(
            condition = "input.town == 'new town'",
            textInput("text", "New Town")
          ),
          # actionButton shall appear if selectizeInput changes, no matter if
          # the town is already known or not
          conditionalPanel(
            condition = paste0("input.town != '", selectedTown, "'"),
            actionButton("entry", "save town")
          ),
          br(),
          hr()
        )
      }
    })
  })
  observeEvent(input$entry, {
    rV$towns$town[selection()] <- ifelse(input$town == "new town",
                                         input$text, input$town)
  })
}

# Run the application
shinyApp(ui = ui, server = server)