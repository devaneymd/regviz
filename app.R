library(shiny)
library(bslib)
library(plotly)
library(car)
library(MASS)
library(GGally)
library(olsrr)
library(corrplot)
library(shinythemes)
library(caret)
library(shinyBS)

ui <- fluidPage(
  theme = shinytheme("yeti"),
  # Application name
  titlePanel("An Overview: Linear Models"),
  # Horizontal line break
  tags$hr(),
  # Creates the sidebar for choosing data and model parameters
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
      uiOutput("factors"),
      conditionalPanel(
        condition = "input.factors",
        actionButton(
          inputId = "update_factors",
          label = "Convert Variables to Factors"
        )
      ),
      conditionalPanel(
        condition = "input.update_factors",
        uiOutput("response"),
        uiOutput("predictors"),
        uiOutput("interactions"),
        uiOutput("transformation"),
        bsTooltip(id = "transformation",
                  title = "Transformation applied only to main effects")
      )
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
                title = "Correlation",
                plotOutput("corr_matrix"),
                h4("Variance Inflation Factor (VIF)"),
                verbatimTextOutput("vif"),
                span("VIF is a statistic which determines the degree of
                     correlation between your model's predictors.
                     A VIF score \\(>\\) 10 indicates multicollinearity
                     is occuring, which will impact your model's reliability."
                )
              ),
              # Summary tab
              tabPanel(
                title = "Summary Stats",
                conditionalPanel(
                  condition = "input.predictors != ''",
                  verbatimTextOutput("summary"),
                  uiOutput("multiple_formula"),
                  h4("Root Mean Squared Error (RMSE)"),
                  span("The standard deviation of the error:"),
                  verbatimTextOutput("rmse"),
                  h4("Coefficient Values for a 95% Confidence Interval:"),
                  verbatimTextOutput("confidence"),
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
              ),
              tabPanel(
                title = "Partial F Test",
                uiOutput("reduced_predictors"),
                span(em("Start by choosing one less predictor in this
                        model than your first model.")),
                conditionalPanel(
                  condition = "input.predictors != '' && input.reduced_predictors !=''",
                  verbatimTextOutput("partial_f"),
                  span("The partial F test models the difference in fit
                       between the models. This can be seen in the
                       difference between each model's", code("RSS"),
                       "(Residual Sum of Squares).",  "Additionally,
                       it will tell us the importance of the particular variable
                       that is being left out in the reduced model.
                       The p-value will determine whether that change
                       is statistically significant.")
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
                plotOutput("standard_residual"),
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
          ),
          # Make a prediction on the data
          tabPanel(
            title = "Predict",
            conditionalPanel(
              condition = "input.predictors != ''",
              h3("Predict Your Response Variable"),
              uiOutput("predict_choice"),
              actionButton(
                inputId = "predict",
                label = "Make Prediction"
              ),
              bsTooltip(id = "predict",
                        title = "Enter data for predictors to make a prediction."),
              uiOutput("predict_mean"),
              verbatimTextOutput("prediction_result"),
              uiOutput("prediction_meaning")
            )
          )
        )
      )
    )
  )
)


server <- function(input, output) {
  # Create a data frame from the user selected data
  data <- reactiveValues(df = NULL)

  observeEvent(input$file, {
    data$df <- read.csv(input$file$datapath)
    updateSelectInput(inputId = "factors", choices = names(data$df))
  })

  observeEvent(input$update_factors, {
    if (!is.null(data$df)) {
      factors <- input$factors
      data$df[factors] <- lapply(data$df[factors], factor)
      str(data$df)
    }
  })

  # Create a drop down box of variables to choose as response
  output$response <- renderUI({
    response <- names(data$df)
    selectInput(
      inputId = "response",
      label = "Select Response",
      choices = response,
      multiple = FALSE
    )
  })

  # Create a group of check boxes to select multiple predictors
  output$predictors <- renderUI({
    predictors <- names(data$df)
    predictors <- predictors[predictors != input$response]
    checkboxGroupInput(
      inputId = "predictors",
      label = "Select Predictors",
      choices = predictors,
      selected = NULL
    )
  })

  # Create a dropdown box for choosing the categorical variables
  output$factors <- renderUI({
    req(data$df)
    factors <- names(data$df)
    factors <- factors[factors != input$response]
    selectInput(
      inputId = "factors",
      label = "Select Categorical Variables",
      choices = factors,
      multiple = TRUE,
      selected = NULL
    )
  })

  # Create a reduced model for partial F test
  output$reduced_predictors <- renderUI({
    req(data$df)
    predictors <- names(data$df)
    predictors <- predictors[predictors != input$response]
    predictors <- c(predictors, input$interactions)
    checkboxGroupInput(
      inputId = "reduced_predictors",
      label = h4("Select Reduced Model Predictors"),
      choices = predictors,
      inline = TRUE,
      selected = NULL
    )
  })

  # Create the linear model to be used throughout
  model <- reactive({
    req(input$predictors, input$response, data$df)
    predictors <- input$predictors

    # Check for interaction effects
    if (!is.null(input$interactions)) {
      # Append interaction terms to predictors
      for (interaction in input$interactions) {
        predictors <- c(predictors, interaction)
      }
    }

    if (input$transformation == "Square Root") {
      predictors <- paste0("sqrt(", predictors, ")")
    } else if (input$transformation == "Natural Logarithm") {
      predictors <- paste0("log(", predictors, ")")
    }

    # Create the formula and model
    lm(
      formula(
        paste(
          input$response, "~",
          paste(predictors, collapse = "+")
        )
      ), data = data$df
    )
  })


  # Choosing interaction effects
  output$interactions <- renderUI({
    req(input$predictors)
    if (length(input$predictors) > 1) {
      interactions <- combn(input$predictors, 2, FUN = paste, collapse = ":")
      checkboxGroupInput(
      inputId = "interactions",
      label = "Select Interaction Effects",
      choices = interactions,
      selected = NULL
      )
    }
  })

  # Creates a scatter plot of the chosen data and fits a regression line
  output$scatter <- renderPlotly({
    req(input$predictors, input$response, data$df)
    if (length(input$predictors) > 1) {
      showNotification("Simple regression only allows for one predictor!",
                       type = "error")
      return(NULL)
    } else if (class(data$df[, input$predictors]) == "factor") {
      showNotification("Cannot plot regression line on a categorical variable!")
      return(NULL)
    }

    # Creating a scatter plot
    plot_ly(
      x = data$df[, input$predictors],
      y = data$df[, input$response],
      type = "scatter",
      mode = "markers"
    ) %>%
      # Adding a regression line
      add_lines(x = data$df[, input$predictors], y = fitted(model())) %>%
      # Formatting the axes and plot
      layout(
        title = paste(input$response, " vs. ", paste(input$predictors, collapse = ", ")),
        xaxis = list(title = input$predictors),
        yaxis = list(title = input$response),
        showlegend = FALSE,
        # Show R^2 on the graph
        annotations = list(
          text = paste("$R{^2}:",
                       round(summary(model())$r.squared, digits = 3),
                       "$"),
          x = 4 * max(data$df[, input$predictors]) / 5,
          y = 4 * max(data$df[, input$response]) / 5,
          showarrow = FALSE,
          font = list(size = 28)
        )
      )
  })

  # Creates a plot of the residuals
  output$residual <- renderPlotly({
    req(input$predictors, input$response, data$df)
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

  # Plot the standardized residuals
  output$standard_residual <- renderPlot({
    req(input$predictors, input$response, data$df)
    plot(model(), which = 1, main = "Standardized Residuals",
         pch = 16, col = "#1f77b4")
  })

  # Create a quantile-quantile plot for residual distribution
  output$qq <- renderPlot({
    req(input$predictors, input$response, data$df)
    qqnorm(resid(model()), pch = 16, col = "#1f77b4")
    qqline(resid(model()), col = "#ff8d29", lwd = 2)
  })

  # Creates a plot of the residual distribution
  output$density <- renderPlot({
    req(input$predictors, input$response, data$df)
    plot(density(resid(model())), col = "#ff8d29", lwd = 2)
  })

  # Creates partial dependent/added variable plots
  output$partial_dep <- renderPlot({
    req(input$predictors, input$response, data$df)
    avPlots(model(), col = "#1f77b4", col.lines = "#ff8d29",
            pch = 16, lwd = 2, ask = FALSE)
  })

  # Prints various model statistics
  output$summary <- renderPrint({
    req(input$predictors, input$response, data$df)
    summary(model())
  })

  # Create an output of 95% confidence intervals
  output$confidence <- renderPrint({
    req(input$predictors, input$response, data$df)
    confint(model())
  })

  # Creates a model with the lowest AIC
  output$best <- renderPrint({
    req(input$response, input$predictors, data$df)
    null_model <- lm(formula(paste(input$response, "~", "1")), data = data$df)
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
    req(input$predictors, input$response, data$df)
    if (length(input$predictors) > 1) {
      return(NULL)
    } else if (class(data$df[, input$predictors]) == "factor") {
      return(NULL)
    }
    # Change equation display based on selected transformation
    transformation <- "x$$"
    interpretation <- ""
    if (input$transformation == "Square Root")
      transformation <- "\\sqrt{x}$$"
    else if (input$transformation == "Natural Logarithm")
      transformation <- "\\ln{(x)}$$"
    else if (input$transformation == "None")
      interpretation <- span(
        "For every one unit increase in ", code(input$predictors), ", ",
        code(input$response), ifelse(model()$coefficients[2] > 0,
                                     "increases by ", "decreases by"),
        model()$coefficients[2], "units."
      )


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
        model()$coefficients[2], transformation
      ),
      interpretation
    )
  })

  # Create a matrix plot of the correlation values of the data
  output$corr_matrix <- renderPlot({
    req(data$df, input$response)
    # Can't have factors in the correlation matrix
    correlation_df <- data$df[, -which(sapply(data$df, class) == "factor")]
    corrplot(cor(correlation_df), method = "square", bg = "#828282")
  })

  # Create a plot that shows Cook's distance for outliers
  output$cooks <- renderPlot({
    req(data$df, input$response, input$predictors)
    plot(model(), pch = 16, col = "#1f77b4")
  })

  # Create an output for Mallow's C_p for model fit
  output$mallows <- renderPrint({
    req(data$df, input$response, input$predictors)
    full_model <- lm(
      formula(
        paste(
          input$response, "~."
        )
      ), data = data$df
    )
    ols_mallows_cp(model(), full_model)
  })

  # Create the model formula for multiple regression
  output$multiple_formula <- renderUI({
    req(input$predictors, input$response, data$df)
    # Get predictor names from the row names of the model summary
    predictor_labels <- rownames(summary(model())$coefficients)
    transformation <- c(paste0("\\mathbf{", predictor_labels, "}"))

    coefficients <- model()$coefficients
    # Start an equation environment with the aligned setting
    equation <- "\\begin{equation}\\begin{aligned}"
    # Create the default equation with just the intercept
    equation <- paste(equation, "\\mathbf{", input$response, "}", " = ",
                      round(coefficients[1], digits = 5))
    # Append the selected terms to the equation, -1 to exclude intercept
    num_predictors <- length(predictor_labels) - 1
    for (i in 1:num_predictors) {
      equation <- paste(equation,
                        ifelse(coefficients[i + 1] > 0, "+", ""),
                        round(coefficients[i + 1], digits = 5),
                        transformation[i + 1])

      # Equation is getting too long, put the rest on a new line
      if (i %% 4 == 0)
        equation <- paste(equation, "\\\\")
    }

    equation <- paste(equation, "\\end{aligned}\\end{equation}")
    withMathJax(equation)
  })

  # Calculate the VIF stat
  output$vif <- renderPrint({
    req(input$response, data$df)
    if (length(input$predictors) != 0)
      return(1 / (1 - summary(model())$r.squared))
    else
      return(NULL)
  })

  # Calculate the partial F test
  output$partial_f <- renderPrint({
    req(data$df, input$response, input$reduced_predictors)

    predictors <- input$reduced_predictors

    if (input$transformation == "Square Root") {
      predictors <- paste0("sqrt(", predictors, ")")
    } else if (input$transformation == "Natural Logarithm") {
      predictors <- paste0("log(", predictors, ")")
    }

    # Create the formula and model
    reduced_model <- lm(
      formula(
        paste(
          input$response, "~",
          paste(predictors, collapse = "+")
        )
      ), data = data$df
    )
    anova(model(), reduced_model)
  })

  # Create transformations of the predictors
  output$transformation <- renderUI({
    req(data$df)
    transformations <- c("None", "Square Root", "Natural Logarithm")
    selectInput(
      inputId = "transformation",
      label = "Apply Transformation",
      choices = transformations,
      selected = NULL
    )
  })

  # Create a checkbox for predicting the mean
  output$predict_mean <- renderUI({
    req(data$df)
    checkboxInput(
      inputId = "predict_mean",
      label = h5("Predict Mean"),
      value = FALSE
    )
  })

  # Calculate RMSE for model fit testing
  output$rmse <- renderPrint({
    req(input$predictors, input$response, data$df)

    predictors <- data$df[, input$predictors, drop = FALSE]
    predictions <- predict(model(), newdata = predictors)
    RMSE(predictions, data$df[[input$response]])
  })

  # Create a tab for allowing user to predict a data point
  output$predict_choice <- renderUI({
    req(input$response, input$predictors, data$df)

    predictor_values <- list()

    for (i in 1:length(input$predictors)) {
      predictor_value <- numericInput(
        inputId = input$predictors[i],
        label = h4(input$predictors[i]),
        value = 0,
        width = "200px"
      )

      predictor_values[[i]] <- predictor_value
    }

    do.call(tagList, predictor_values)
  })

  # Wait for make prediction button to be clicked and then make prediction
  observeEvent(input$predict, {
    predictor_values <- sapply(input$predictors,
                               function(predictor) input[[predictor]])

    newdata <- data.frame(t(predictor_values))
    colnames(newdata) <- input$predictors

    if (input$predict_mean)
      interval <- "confidence"
    else
      interval <- "predict"

    make_prediction <- predict(model(), newdata = newdata, interval = interval)

    output$prediction_result <- renderPrint({
      make_prediction
    })

    output$prediction_meaning <- renderUI({
      span(code("fit"), "tells us the prediction made by model.",
           code("lwr"), " and ", code("upr"),
           "tell us the 95% confidence interval."
      )
    })
  })
}

shinyApp(ui = ui, server = server)
