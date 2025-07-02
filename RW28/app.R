# Load required libraries
library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Model Builder: OLS vs GLM Gamma (log link)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File (.xlsx)", accept = ".xlsx"),
      textInput("sheet", "Sheet Name", value = "plot data"),
      
      uiOutput("stdy_ui"),
      uiOutput("yst_ui"),
      
      uiOutput("response_ui"),
      uiOutput("predictors_ui"),
      
      actionButton("run_models", "Run Models")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("OLS Model",
                 verbatimTextOutput("model_summary_ols"),
                 plotOutput("diagnostic_plots_ols")
        ),
        tabPanel("GLM Gamma (log link)",
                 verbatimTextOutput("model_summary_gamma"),
                 plotOutput("diagnostic_plots_gamma")
        ),
        tabPanel("Model Comparison (AIC)",
                 verbatimTextOutput("model_comparison")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Load and prepare data
  data_input <- reactive({
    req(input$file)
    df <- read_excel(input$file$datapath, sheet = input$sheet) |>
      select(stdy, plot, yst, cump, totp, dbh, ht, vol) |>
      mutate(
        estabp = totp - cump,
        TRT_CODE = substr(plot, 2, 4)
      )
    df
  })
  
  # Dynamic UI for stdy selection
  output$stdy_ui <- renderUI({
    req(data_input())
    selectInput("stdy_filter", "Select Study (stdy)", choices = unique(data_input()$stdy))
  })
  
  # Dynamic UI for yst based on selected stdy
  output$yst_ui <- renderUI({
    req(input$stdy_filter)
    df <- data_input() |> filter(stdy == input$stdy_filter)
    selectInput("yst_filter", "Select Year (yst)", choices = unique(df$yst))
  })
  
  # Filtered data based on stdy and yst
  filtered_data <- reactive({
    req(input$stdy_filter, input$yst_filter)
    data_input() |> filter(stdy == input$stdy_filter, yst == input$yst_filter)
  })
  
  # Response variable selection limited to dbh, ht, vol
  output$response_ui <- renderUI({
    req(filtered_data())
    selectInput("response", "Select Response Variable", 
                choices = c("dbh", "ht", "vol"), 
                selected = "ht")
  })
  
  # Predictor variable selection limited to stdy, plot, yst, cump, totp, estabp
  output$predictors_ui <- renderUI({
    req(filtered_data(), input$response)
    choices <- c("stdy", "plot", "yst", "cump", "totp", "estabp")
    checkboxGroupInput("predictors", "Select Predictor Variables", choices = choices)
  })
  
  # Fit both models
  models_result <- eventReactive(input$run_models, {
    req(input$response, input$predictors)
    
    # Build formula text without interaction
    formula_text <- paste(input$response, "~", paste(input$predictors, collapse = " + "))
    
    df <- filtered_data()
    
    model_ols <- lm(as.formula(formula_text), data = df)
    model_gamma <- glm(as.formula(formula_text), data = df, family = Gamma(link = "log"))
    
    list(ols = model_ols, gamma = model_gamma)
  })
  
  # Display OLS summary
  output$model_summary_ols <- renderPrint({
    req(models_result())
    summary(models_result()$ols)
  })
  
  # OLS diagnostic plots
  output$diagnostic_plots_ols <- renderPlot({
    req(models_result())
    par(mfrow = c(2, 2))
    plot(models_result()$ols)
  })
  
  # Display Gamma model summary
  output$model_summary_gamma <- renderPrint({
    req(models_result())
    summary(models_result()$gamma)
  })
  
  # Gamma diagnostic plots
  output$diagnostic_plots_gamma <- renderPlot({
    req(models_result())
    par(mfrow = c(2, 2))
    plot(models_result()$gamma)
  })
  
  # Model AIC comparison with interpretation
  output$model_comparison <- renderPrint({
    req(models_result())
    aic_values <- AIC(models_result()$ols, models_result()$gamma)
    print(aic_values)
    
    ols_aic <- aic_values$AIC[1]
    gamma_aic <- aic_values$AIC[2]
    
    cat("\nModel Comparison Conclusion:\n")
    if (ols_aic < gamma_aic) {
      cat("OLS model has the lower AIC and is therefore preferred based on AIC.\n")
    } else if (gamma_aic < ols_aic) {
      cat("GLM Gamma (log link) model has the lower AIC and is therefore preferred based on AIC.\n")
    } else {
      cat("Both models have identical AIC values.\n")
    }
  })
}

shinyApp(ui, server)

