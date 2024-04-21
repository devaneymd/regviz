library(shiny)
library(bslib)
library(plotly)
library(car)
library(MASS)
library(GGally)

ui <- page_sidebar(
  withMathJax(),
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
        # Statistics tab
        tabPanel(
          title = "Statistics",
          tabsetPanel(
            tabPanel(
              title = "Correlation Matrix",
              verbatimTextOutput("corr_matrix")
            ),
            # Summary tab
            tabPanel(
              title = "Summary Stats",
              verbatimTextOutput("summary"),
              conditionalPanel(
                condition = "output.summary",
                checkboxInput(
                  inputId = "interaction",
                  label = "Interaction Effects",
                  value = FALSE
                ),
                uiOutput("multiple_formula")
              )
            ),
            tabPanel(
              title = "Optimize AIC",
              verbatimTextOutput("best")
            )
          )
        ),
        # Plot tab
        tabPanel(
          title = "Plots",
          tabsetPanel(
            tabPanel(
              title = "Pairwise",
              plotOutput("pair")
            ),
            # Regression tab
            tabPanel(
              title = "Simple Regression",
              plotlyOutput("scatter"),
              uiOutput("simple_formula")
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


  model <- reactive({
    req(input$predictors, input$response, df())
    lm(
      formula(
        paste(
          input$response, "~",
          paste(input$predictors,
                collapse = ifelse(input$interaction, "*", "+"))
        )
      ), data = df()
    )
  })

  # Creates a scatter plot of the chosen data and fits a regression line
  output$scatter <- renderPlotly({
    req(input$predictors, input$response, df())
    if (length(input$predictors) > 1) {
      showNotification("Simple regression only allows for one predictor!",
                       type = "error")
      return(NULL)
    }
    # Creating a scatter plot
    plot_ly(
      x = df()[, input$predictors],
      y = df()[, input$response],
      type = "scatter",
      mode = "markers"
    ) %>%
      # Adding a regression line
      add_lines(x = df()[, input$predictors], y = fitted(model())) %>%
      # Formatting the axes and plot
      layout(
        title = paste(input$response, " vs. ", paste(input$predictors, collapse = ", ")),
        xaxis = list(title = input$predictors),
        yaxis = list(title = input$response),
        showlegend = FALSE
      )
  })

  # Creates a plot of the residuals
  output$residual <- renderPlotly({
    req(input$predictors, input$response, df())
    if (length(input$predictors) > 1) {
      showNotification("Simple regression only allows for one predictor!",
                       type = "error")
      return(NULL)
    }
    plot_ly(
      x = fitted(model()),
      y = resid(model()),
      type = "scatter",
      mode = "markers"
    ) %>%
      layout(
        title = "Residuals",
        xaxis = list(title = "Model"),
        yaxis = list(title = "Residuals"),
        showlegend = FALSE
      )
  })

  # Create a quantile-quantile plot for residual distribution
  output$qq <- renderPlot({
    req(input$predictors, input$response, df())
    if (length(input$predictors) > 1) {
      return(NULL)
    }
    qqnorm(resid(model()), pch = 16, col = "#1f77b4")
    qqline(resid(model()), col = "#ff8d29", lwd = 2)
  })

  # Creates a plot of the residual distribution
  output$density <- renderPlot({
    req(input$predictors, input$response, df())
    if (length(input$predictors) > 1) {
      return(NULL)
    }
    plot(density(resid(model())), col = "#ff8d29", lwd = 2)
  })

  # Creates partial dependent/added variable plots
  output$partial_dep <- renderPlot({
    req(input$predictors, input$response, df())
    avPlots(model(), col = "#1f77b4", col.lines = "#ff8d29",
            pch = 16, lwd = 2, ask = FALSE)
  })

  # Prints various model statistics
  output$summary <- renderPrint({
    req(input$predictors, input$response, df())
    summary(model())
  })

  # Creates a model with the lowest AIC
  output$best <- renderPrint({
    req(input$response, input$predictors, df())
    null_model <- lm(formula(paste(input$response, "~", "1")), data = df())
    forward_select <- stepAIC(null_model,
                              paste(input$response, "~",
                                    paste(input$predictors,
                                          collapse = "*")),
                              direction = "forward",
                              trace = FALSE)
    forward_select$call
    forward_select$anova
  })

  # Creates the formula y_hat = beta_0 + beta_1*x
  output$simple_formula <- renderUI({
    req(input$predictors, input$response, df())
    if (length(input$predictors) > 1) {
      return(NULL)
    }
    withMathJax(
      paste(
        "$$\\beta_0=", model()$coefficients[1],
        "\\quad\\beta_1=", model()$coefficients[2], "$$"
      ),
      paste(
        "$$\\hat{y}=", model()$coefficients[1],
        ifelse(model()$coefficients[2] > 0, "+", ""),
        model()$coefficients[2], "x$$"
      )
    )
  })

  output$corr_matrix <- renderPrint({
    req(df())
    numeric_cols <- sapply(df(), is.numeric)
    numeric_data <- df()[, numeric_cols]
    cor(numeric_data)
  })

  output$pair <- renderPlot({
    req(df())
    numeric_cols <- sapply(df(), is.numeric)
    numeric_data <- df()[, numeric_cols]
    ggpairs(numeric_data)
  })

}

shinyApp(ui = ui, server = server)
