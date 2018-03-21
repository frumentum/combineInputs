library(shiny)
library(DT)

startData <- data.frame(
  town = c("Berlin", "London", "Paris"),
  population = c(3711930, 9787426, 12142802),
  popCountry = c(82500000, 54000000, 67000000),
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
    if (!is.null(input$table_rows_selected)) {
      return(rV$towns[input$table_rows_selected, ]) 
    } else return(0)
  })
  
  # if a row is selected, one will get the possibility to edit the town;
  # if no row is selected (or it's deselected), edit mode shall disappear
  observeEvent(selection(), priority = 1, {
    output$editDB <- renderUI({
      if (is.data.frame(selection())) {
        # which town is selected? create character string
        selectedTown <- selection()$town
        # which population is selected?
        selectedPop <- selection()$population
        
        # create UI for editing datatable
        tagList(
          column(4,
            selectizeInput("town", "Town",
                           choices = unique(rV$towns$town),
                           select = selectedTown,
                           options = list(create = TRUE)),
            # actionButton shall appear if selectizeInput changes, no matter if
            # the town is already known or not
            # conditionalPanel(
            #   condition = paste0("input.town != '", selectedTown, "' | ",
            #                      "input.population != '", selectedPop, "'"),
            #   actionButton("entry", "save changes")
            # ),
            br(),
            hr()
          ),
          column(4,
            sliderInput(
              "population", "Population", 
              min = 0,
              max = selection()$popCountry,
              value = selectedPop
            )
          )
        )
      }
    })
  })
  
  observe(priority = 0, {
    newData <- data.frame(
      town = input$town,
      population = input$population,
      stringsAsFactors = FALSE
    )
    isolate(
      rV$towns[input$table_rows_selected, c("town", "population")] <- newData
    )
      
  })
}

# Run the application
shinyApp(ui = ui, server = server)