# Load necessary packages
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(officer)
library(flextable)

# Define the UI layout
ui <- fluidPage(
  titlePanel("Data Analysis App for Uploaded Files"),
  
  # Sidebar layout with input controls
  sidebarLayout(
    sidebarPanel(
      # File input for browsing files in the working directory
      fileInput("fileInput", "Choose a CSV, TXT, or XLSX File:",
                accept = c(".csv", ".txt", ".xlsx")),
      # Download button for the report
      downloadButton("downloadReport", "Download Full Report (Word)")
    ),
    
    # Main panel to display outputs
    mainPanel(
      uiOutput("dynamicOutputs")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Reactive expression to read the uploaded file
  dataset <- reactive({
    req(input$fileInput)
    
    file <- input$fileInput
    ext <- tools::file_ext(file$datapath)
    dataset_name <- tools::file_path_sans_ext(file$name)
    
    if (ext == "csv") {
      data <- read.csv(file$datapath, stringsAsFactors = FALSE)
    } else if (ext == "txt") {
      data <- read.table(file$datapath, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
    } else if (ext == "xlsx") {
      data <- read_excel(file$datapath)
    } else {
      stop("Unsupported file type")
    }
    
    assign(dataset_name, data, envir = .GlobalEnv)
    data
  })
  
  # Render dynamic outputs for each variable
  output$dynamicOutputs <- renderUI({
    data <- dataset()
    output_list <- lapply(names(data), function(var) {
      var_title <- paste("Analysis for Variable:", var)
      if (is.character(data[[var]]) || is.factor(data[[var]])) {
        tagList(
          h3(var_title),
          plotOutput(outputId = paste0("plot_", var)),
          h4(paste("Frequency Table of", var)),
          tableOutput(outputId = paste0("table_", var))
        )
      } else if (is.numeric(data[[var]])) {
        tagList(
          h3(var_title),
          plotOutput(outputId = paste0("hist_", var)),
          h4(paste("Summary Statistics of", var)),
          tableOutput(outputId = paste0("summary_", var))
        )
      }
    })
    do.call(tagList, output_list)
  })
  
  # Create plots and tables for each variable
  observe({
    data <- dataset()
    for (var in names(data)) {
      local({
        v <- var
        if (is.character(data[[v]]) || is.factor(data[[v]])) {
          output[[paste0("plot_", v)]] <- renderPlot({
            ggplot(data, aes_string(x = v)) +
              geom_bar(fill = "skyblue", color = "black") +
              labs(title = paste("Bar Chart of", v), x = v, y = "Frequency") +
              theme_minimal()
          })
          output[[paste0("table_", v)]] <- renderTable({
            as.data.frame(table(data[[v]])) %>% setNames(c("Category", "Frequency"))
          })
        } else if (is.numeric(data[[v]])) {
          output[[paste0("hist_", v)]] <- renderPlot({
            ggplot(data, aes_string(x = v)) +
              geom_histogram(fill = "lightgreen", color = "black", bins = 10) +
              labs(title = paste("Histogram of", v), x = v, y = "Frequency") +
              theme_minimal()
          })
          output[[paste0("summary_", v)]] <- renderTable({
            summary_data <- summary(data[[v]])
            data.frame(Statistic = names(summary_data), Value = as.numeric(summary_data))
          })
        }
      })
    }
  })
  
  # Download handler for generating the report
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("EDA_Report_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      data <- dataset()
      doc <- read_docx()
      doc <- body_add_par(doc, "Exploratory Data Analysis Report", style = "heading 1")
      
      for (var in names(data)) {
        doc <- body_add_par(doc, paste("Analysis for Variable:", var), style = "heading 2")
        
        if (is.character(data[[var]]) || is.factor(data[[var]])) {
          # Add bar chart for categorical variables
          plot_path <- tempfile(fileext = ".png")
          png(plot_path, width = 800, height = 600) # Adjust size as needed
          print(ggplot(data, aes_string(x = var)) +
                  geom_bar(fill = "skyblue", color = "black") +
                  labs(title = paste("Bar Chart of", var), x = var, y = "Frequency") +
                  theme_minimal())
          dev.off()
          
          # Check if the image was created successfully before adding it
          if (file.exists(plot_path)) {
            print(paste("Image created at:", plot_path)) # Debugging print statement
            doc <- body_add_img(doc, src = plot_path, width = 5, height = 4)
          } else {
            print("Image creation failed") # Debugging print statement
            doc <- body_add_par(doc, "Plot could not be generated.", style = "normal")
          }
          
          # Add frequency table
          freq_table <- as.data.frame(table(data[[var]])) %>% setNames(c("Category", "Frequency"))
          doc <- body_add_flextable(doc, flextable(freq_table))
        } else if (is.numeric(data[[var]])) {
          # Add histogram for numeric variables
          plot_path <- tempfile(fileext = ".png")
          png(plot_path, width = 800, height = 600) # Adjust size as needed
          print(ggplot(data, aes_string(x = var)) +
                  geom_histogram(fill = "lightgreen", color = "black", bins = 10) +
                  labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
                  theme_minimal())
          dev.off()
          
          # Check if the image was created successfully before adding it
          if (file.exists(plot_path)) {
            print(paste("Image created at:", plot_path)) # Debugging print statement
            doc <- body_add_img(doc, src = plot_path, width = 5, height = 4)
          } else {
            print("Image creation failed") # Debugging print statement
            doc <- body_add_par(doc, "Plot could not be generated.", style = "normal")
          }
          
          # Add summary statistics
          summary_data <- summary(data[[var]])
          summary_table <- data.frame(Statistic = names(summary_data), Value = as.numeric(summary_data))
          doc <- body_add_flextable(doc, flextable(summary_table))
        }
      }
      
      print(doc, target = file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
