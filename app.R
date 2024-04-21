library(shiny)
library(bslib)
library(plotly)
library(car)
library(MASS)
library(GGally)
library(olsrr)
library(corrplot)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("yeti"),
  # Application name
  titlePanel("Linear Regression"),

  # Horizontal line break
  tags$hr(),

  sidebarLayout(
    sidebarPanel = sidebarPanel(
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
      helpText("Note: Remove NA's and row names from your file."),
      # UI for choosing response and predictor variables
      uiOutput("response"),
      uiOutput("predictors")
    ),

    mainPanel = mainPanel(
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
                plotOutput("corr_matrix")
              ),
              # Summary tab
              tabPanel(
                title = "Summary Stats",
                conditionalPanel(
                  condition = "input.predictors != ''",
                  verbatimTextOutput("summary"),
                  uiOutput("multiple_formula"),
                  conditionalPanel(
                    condition = "output.summary",
                    checkboxInput(
                      inputId = "interaction",
                      label = "Interaction Effects",
                      value = FALSE
                  ),
                  h4("Values for a 95% Confidence Interval:"),
                  verbatimTextOutput("confidence"),
                  )
                )
              ),
              tabPanel(
                title = "Optimization",
                conditionalPanel(
                  condition = "input.predictors != ''",
                  h4("Akaike's Information Criterion (AIC)"),
                  verbatimTextOutput("best"),
                  span("The AIC estimates the amount of information lost
                       proportional to the number of predictors
                       in the model. A model with an AIC of \\(X\\) and
                       \\(p+1\\) predictors would need an AIC that is at
                       least 10 units lower than a model with \\(p\\)
                       predictors to be a better fit than the model
                       with \\(p\\) predictors."),
                  h4(withMathJax(paste("Mallow's", "\\(C_p\\)"))),
                  verbatimTextOutput("mallows"),
                  span("Get this number as close to
                       \\(p+1\\) as possible for the best model fit.")
                )
              )
            )
          ),
          # Plot tab
          tabPanel(
            title = "Plots",
            tabsetPanel(
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
                              plotOutput("density")),
                  plotOutput("cooks")
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
        showlegend = FALSE,
        annotations = list(
          text = paste("$R{^2}:",
                       round(summary(model())$r.squared, digits = 3),
                       "$"),
          x = 4 * max(df()[, input$predictors]) / 5,
          y = 4 * max(df()[, input$response]) / 5,
          showarrow = FALSE,
          font = list(size = 28)
        )
      )
  })


  # Creates a plot of the residuals
  output$residual <- renderPlotly({
    req(input$predictors, input$response, df())
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
    qqnorm(resid(model()), pch = 16, col = "#1f77b4")
    qqline(resid(model()), col = "#ff8d29", lwd = 2)
  })

  # Creates a plot of the residual distribution
  output$density <- renderPlot({
    req(input$predictors, input$response, df())
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

  output$confidence <- renderPrint({
    req(input$predictors, input$response, df())
    confint(model())
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
      h4(
        "The Regression Equation is:"
      ),
      paste(
        "$$\\beta_0=", model()$coefficients[1],
        "\\quad\\beta_1=", model()$coefficients[2], "$$"
      ),
      paste(
        "$$\\hat{y}=", model()$coefficients[1],
        ifelse(model()$coefficients[2] > 0, "+", ""),
        model()$coefficients[2], "x$$"
      ),
      paste(
        "For every one unit increase in ", input$predictors, ", ", input$response, ifelse(model()$coefficients[2] > 0, "increases by ", "decreases by"), model()$coefficients[2], "units.")
      )
  })

  output$corr_matrix <- renderPlot({
    req(df(), input$response)
    corrplot(cor(df()), method = "number", bg = "#8c8a87")
  })

  output$cooks <- renderPlot({
    req(df(), input$response, input$predictors)
    plot(model())
  })

  output$mallows <- renderPrint({
    req(df(), input$response, input$predictors)
    full_model <- lm(
      formula(
        paste(
          input$response, "~."
        )
      ), data = df()
    )
    ols_mallows_cp(model(), full_model)
  })

  output$multiple_formula <- renderUI({
    req(input$predictors, input$response, df())

    coefficients <- model()$coefficients
    # Start an equation environment with the aligned setting
    equation <- "\\begin{equation}\\begin{aligned}"
    # Create the default equation with just the intercept
    equation <- paste(equation, "\\mathbf{ ", input$response, "}", " = ", round(coefficients[1], digits = 5))
    # Append the selected terms to the equation
    for (i in 1:length(input$predictors)) {
      equation <- paste(equation,
                        ifelse(coefficients[i + 1] > 0, "+", ""),
                        round(coefficients[i + 1], digits = 5),
                        "\\mathbf{", input$predictors[i], "}")

      # Equation is getting too long, put the rest on a new line
      if (i %% 4 == 0)
        equation <- paste(equation, "\\\\")
    }

    equation <- paste(equation, "\\end{aligned}\\end{equation}")
    withMathJax(equation)
  })


}

shinyApp(ui = ui, server = server)
