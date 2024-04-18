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

  tableOutput("contents")
)


server <- function(input, output) {
  output$contents <- renderTable({
    # Must have a file to read
    req(input$file)
    df <- read.csv(
      input$file$datapath
    )
    return(df)
  })
}

shinyApp(ui = ui, server = server)
