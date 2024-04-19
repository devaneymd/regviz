library(shiny)
library(bslib)
library(plotly)

ui <- page_sidebar(

  # Application name
  titlePanel("Regression Exploration"),

  # Horizontal line break
  tags$hr(),

  sidebar = sidebar(
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

    # UI for choosing response and predictor variables
    # verbatimTextOutput("summary"),
    uiOutput("response"),
    uiOutput("predictors")
  ),
    plotlyOutput("scatter")
)


server <- function(input, output) {
  # Create a data frame from the user selected data
  df <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  # Create a drop down box of variables to choose as response
  output$response <- renderUI({
    response <- names(df())
    selectInput(
      inputId = "response",
      label = "Select Response",
      choices = response,
      multiple = FALSE
    )
  })

  # Create a group of check boxes to select multiple predictors
  output$predictors <- renderUI({
    predictors <- names(df())
    predictors <- predictors[predictors != input$response]
    checkboxGroupInput(
      inputId = "predictors",
      label = "Select Predictors",
      choices = predictors,
      selected = NULL
    )
  })

  output$scatter <- renderPlotly({
    req(input$predictors, input$response, df())
    linear <- lm(
      as.formula(
        paste(
          input$response, "~",
          paste(input$predictors, collapse = "+")
        )
      ), data = df()
    )
    plot_ly(
      x = df()[, input$predictors],
      y = df()[, input$response],
      type = "scatter",
      mode = "markers"
    ) %>%
    add_lines(x = df()[, input$predictors], y = fitted(linear)) %>%
    layout(
      xaxis = list(title = input$predictors),
      yaxis = list(title = input$response),
      showlegend = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)
