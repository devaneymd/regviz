library(shiny)
library(bslib)
library(plotly)
library(car)

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
    uiOutput("response"),
    uiOutput("predictors")
  ),

  mainPanel(
    # Only render the panels after the data is selected
    conditionalPanel(
      condition = "input.response",
      # Main tab navigation
      tabsetPanel(
        id = "tabsetPanelID",
        type = "pills",
        # Plot tab
        tabPanel(
          title = "Plots",
          tabsetPanel(
            # Regression tab
            tabPanel(
              title = "Simple Regression",
              plotlyOutput("scatter")
            ),
            # Residuals tab
            tabPanel(
              title = "Residuals",
               plotlyOutput("residual"),
               fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                 plotOutput("qq"),
                 plotOutput("density"))
              )
            ),
            tabPanel(
              title = "Partial Dependence",
              plotOutput("partial_dep")
            )
          )
        )
      )
    )
  )
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

  # Creates a scatter plot of the chosen data and fits a regression line
  output$scatter <- renderPlotly({
    # Creating the model from user choices
    req(input$predictors, input$response, df())
    tryCatch({
      linear <- lm(
        formula(
          paste(
            input$response, "~", input$predictors, collapse = " "
          )
        ), data = df()
      )
      # Creating a scatter plot
      plot_ly(
        x = df()[, input$predictors],
        y = df()[, input$response],
        type = "scatter",
        mode = "markers"
      ) %>%
        # Adding a regression line
        add_lines(x = df()[, input$predictors], y = fitted(linear)) %>%
        # Formatting the axes and plot
        layout(
          title = paste(input$response, " vs. ", paste(input$predictors, collapse = ", ")),
          xaxis = list(title = input$predictors),
          yaxis = list(title = input$response),
          showlegend = FALSE
        )
    }, error =  function(e) {
        showNotification("Simple regression only allows for one predictor!", type = "error")
        return(NULL)
    })
  })

  output$residual <- renderPlotly({
    req(input$predictors, input$response, df())
    tryCatch({
      linear <- lm(
        formula(
          paste(
            input$response, "~", input$predictors, collapse = " "
          )
        ), data = df()
      )

      plot_ly(
        x = fitted(linear),
        y = resid(linear),
        type = "scatter",
        mode = "markers"
      ) %>%
        layout(
          title = "Residuals",
          xaxis = list(title = "Model"),
          yaxis = list(title = "Residuals"),
          showlegend = FALSE
        )
    }, error =  function(e) {
        showNotification(ui = "Simple regression only allows for one predictor!",
                         type = "error", duration = 2)
        return(NULL)
    })
  })

  output$qq <- renderPlot({
    req(input$predictors, input$response, df())
    tryCatch({
      linear <- lm(
        formula(
          paste(
            input$response, "~", input$predictors, collapse = " "
          )
        ), data = df()
      )
      qqnorm(resid(linear), pch = 16, col = "#1f77b4")
      qqline(resid(linear), col = "#ff8d29", lwd = 2)
    }, error = function(e) {
        return(NULL)
    })
  })

  output$density <- renderPlot({
    req(input$predictors, input$response, df())
    tryCatch({
      linear <- lm(
        formula(
          paste(
            input$response, "~", input$predictors, collapse = " "
          )
        ), data = df()
      )
      plot(density(resid(linear)), col = "#ff8d29", lwd = 2)
  }, error = function(e) {
      return(NULL)
    })
  })

  output$partial_dep <- renderPlot({
    req(input$predictors, input$response, df())
    linear <- lm(
      formula(
        paste(
          input$response, "~", paste(input$predictors, collapse = "+"))
        ), data = df()
      )
    avPlots(linear, col = "#1f77b4", col.lines = "#ff8d29",
            pch = 16, lwd = 2, ask = FALSE)
  })
}

shinyApp(ui = ui, server = server)
