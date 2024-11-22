# Install necessary packages (if not already installed)
if (!require(shiny)) install.packages("shiny")
if (!require(car)) install.packages("car")
if (!require(DT)) install.packages("DT")

# Load libraries
library(shiny)
library(DT)
library(car)

# Define UI
ui <- fluidPage(
  titlePanel("Multiple Linear Regression Shiny App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("var_select"),  # For selecting variables
      actionButton("run_model", "Run Regression"),
      h4("Model Refinement"),
      checkboxInput("forward", "Forward Selection"),
      checkboxInput("backward", "Backward Elimination"),
      actionButton("refine_model", "Refine Model")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Dataset Overview", DTOutput("dataset")),
        tabPanel("Regression Summary", verbatimTextOutput("reg_summary")),
        tabPanel("Visualization", plotOutput("plots")),
        tabPanel("Diagnostics", 
                 h4("Cook's Distance"),
                 plotOutput("cooks_plot"),
                 h4("Variance Inflation Factor (VIF)"),
                 verbatimTextOutput("vif_output"),
                 h4("Residual Analysis"),
                 plotOutput("residuals_plot")
        ),
        tabPanel("Prediction", uiOutput("prediction_ui"), tableOutput("prediction_result"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive dataset
  dataset <- reactive({
    req(input$file)
    data <- read.csv(input$file$datapath)
    return(data)
  })
  
  # Display dataset
  output$dataset <- renderDT({
    req(dataset())
    datatable(dataset())
  })
  
  # Dynamically generate variable selection UI with numeric-only variables
  output$var_select <- renderUI({
    req(dataset())
    data <- dataset()
    numeric_vars <- names(data)[sapply(data, is.numeric)]  # Select only numeric columns
    tagList(
      selectInput("response_var", "Select Response Variable (Y)", numeric_vars),
      selectizeInput("predictor_vars", "Select Predictor Variables (X)", numeric_vars, multiple = TRUE)
    )
  })
  
  # Reactive value to store the model
  rv <- reactiveValues(model = NULL, refined_model = NULL)
  
  # Run initial regression model
  observeEvent(input$run_model, {
    req(input$response_var, input$predictor_vars)
    data <- dataset()
    formula <- as.formula(
      paste(input$response_var, "~", paste(input$predictor_vars, collapse = " + "))
    )
    rv$model <- lm(formula, data = data)
    rv$refined_model <- NULL  # Clear the refined model when a new model is run
  })
  
  # Display regression summary
  output$reg_summary <- renderPrint({
    req(rv$model)
    summary(rv$model)
  })
  
  # Visualize regression results
  output$plots <- renderPlot({
    req(rv$model)
    par(mfrow = c(2, 2))
    plot(rv$model)
  })
  
  # Model refinement
  observeEvent(input$refine_model, {
    req(input$response_var, input$predictor_vars, rv$model)
    data <- dataset()
    full_formula <- as.formula(
      paste(input$response_var, "~", paste(input$predictor_vars, collapse = " + "))
    )
    if (input$forward) {
      rv$refined_model <- step(rv$model, direction = "forward")
    } else if (input$backward) {
      rv$refined_model <- step(rv$model, direction = "backward")
    } else {
      rv$refined_model <- rv$model
    }
  })
  
  # Diagnostics: Cook's Distance
  output$cooks_plot <- renderPlot({
    req(rv$model)
    cooks <- cooks.distance(rv$model)
    plot(cooks, type = "h", main = "Cook's Distance", xlab = "Observations", ylab = "Cook's Distance")
    abline(h = 4/(nrow(dataset())), col = "red", lty = 2)  # Threshold line
  })
  
  # Diagnostics: Variance Inflation Factor (VIF)
  output$vif_output <- renderPrint({
    req(rv$model)
    vif_values <- vif(rv$model)
    vif_values
  })
  
  # Diagnostics: Residual Analysis
  output$residuals_plot <- renderPlot({
    req(rv$model)
    par(mfrow = c(1, 1))
    plot(residuals(rv$model), main = "Residual Analysis", xlab = "Index", ylab = "Residuals")
    abline(h = 0, col = "red", lty = 2)
  })
  
  # Predictions
  output$prediction_ui <- renderUI({
    req(rv$model)
    predictors <- input$predictor_vars
    tagList(
      h4("Enter Values for Prediction"),
      lapply(predictors, function(p) {
        numericInput(paste0("pred_", p), paste("Value for", p), value = NA)
      }),
      actionButton("predict", "Make Prediction")
    )
  })
  
  output$prediction_result <- renderTable({
    req(input$predict, rv$model)
    predictors <- input$predictor_vars
    
    # Create a new data frame with user inputs for prediction
    new_data <- data.frame(
      lapply(predictors, function(p) {
        input_value <- input[[paste0("pred_", p)]]
        if (is.null(input_value) || is.na(input_value)) 0 else input_value  # Default to 0 if missing
      })
    )
    colnames(new_data) <- predictors  # Ensure column names match
    
    # Ensure types are consistent with training data
    for (col in names(new_data)) {
      if (!is.numeric(new_data[[col]])) {
        new_data[[col]] <- as.numeric(new_data[[col]])
      }
    }
    
    # Perform prediction
    model_to_use <- if (!is.null(rv$refined_model)) rv$refined_model else rv$model
    pred <- predict(model_to_use, newdata = new_data)
    data.frame(Predicted_Value = pred)
  }, rownames = FALSE)
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
