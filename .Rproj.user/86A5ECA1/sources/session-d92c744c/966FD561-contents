library(shiny)
library(bslib)

ui <- fluidPage(

  # Application name
  titlePanel("Regression Exploration"),

  # Horizontal line break
  tags$hr(),


  # Loading in the data for regression (csv)
  fileInput(
    inputId = "file",
    label = "Choose Your Data",
    # Can only upload 1 file
    multiple = FALSE,
    # Filters file explorer to csv/txt files
    accept = c(
      "text/csv",
      "text/comma-separated-values,text/plain",
      ".csv"
    )
  ),

  # Testing
  verbatimTextOutput("summary"),
  checkboxGroupInput(inputId = "predictors", label = NULL)

)


server <- function(input, output) {
  # Create a data frame from the user selected data
  df <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  observe({
    predictors <- names(df())
    updateCheckboxGroupInput(inputId = "predictors", label = "Select Predictors", choices = predictors, selected = NULL)
  })
}

shinyApp(ui = ui, server = server)
