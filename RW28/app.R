# Load required libraries
library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggplot2)

# Labels used for plots

response_choices <- c("Height" = "ht", "DBH" = "dbh", "Volume" = "vol")

color_choices <- c(
  "None" = "None",
  "Treatment Code" = "TRT_CODE",
  "Plot (factor)" = "plot_fac",
  "Study (factor)" = "stdy_fac"
)

label_lookup <- c(
  ht = "Height",
  dbh = "DBH",
  vol = "Volume",
  TRT_CODE = "Treatment Code",
  plot_fac = "Plot",
  stdy_fac = "Study"
)



ui <- fluidPage(
  titlePanel("Model Builder: OLS vs GLM Gamma (log link)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file",
                label = "Upload Excel File (.xlsx)",
                accept = ".xlsx"),
      textInput(inputId = "sheet",
                label = "Sheet Name",
                value = "plot data"),

      # Makes the Data Exploration tab sidebar different than the rest (lots of NA inputs)
      conditionalPanel(
        condition = "input.main_tabs == 'Data Exploration'",
        helpText("Plot 1 Options:"),
        selectInput(inputId = "response_var",
                    label = "Select response variable:", 
                    choices = response_choices,
                    selected = "None"),
        
        selectInput(inputId = "color_var",
                    label = "Color plotted points by:",
                    choices = color_choices,
                    selected = "None"),
        
        conditionalPanel(
          condition = "input.color_var != 'None'",
          checkboxGroupInput(inputId = "var_on_plot",
                             label = "Show the following on plot:",
                             choices = NULL,
                             selected = NULL)),
        
        helpText("Plot 2 Options:"),
        helpText("Plot 3 Options:"),
        selectInput(inputId = "xvar",
                    label = "X-axis variable for Interactive Plot:",
                    choices = NULL),  # we'll update choices server-side
        selectInput(inputId = "yvar",
                    label = "Y-axis variable for Interactive Plot:",
                    choices = NULL)
      ),
      
      # Makes the rest of the tabs have the identical sidebar initially put into the app
      conditionalPanel(
        condition = "input.main_tabs != 'Data Exploration'",
        uiOutput("stdy_ui"),
        uiOutput("yst_ui"),
        
        uiOutput("response_ui"),
        uiOutput("predictors_ui"),
        
        actionButton("run_models", "Run Models")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Data Exploration",
                 h3("Plot 1: Response Over Time"),
                 plotOutput("resp_over_time"),
                 hr(),
                 h3("Plot 2: Treatment - Control"),
                 plotOutput("treat_minus_cntrl"),
                 hr(),
                 h3("Plot 3: Interactive"),
                 plotOutput("custom_plot")),
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
        TRT_CODE = substr(plot, 2, 4),
        stdy_fac = as.factor(stdy),
        plot_fac = as.factor(plot)
      )
    
    df
  })
  
  # Dynamically getting the options to include the "color by" variable - plot 1
  observeEvent({
    input$color_var
    data_input()
  }, {
    req(data_input(), input$color_var)
    
    # Only update checkboxes if color_var is not "None"
    if (input$color_var != "None") {
      var_vals <- as.character(data_input()[[input$color_var]])
      var_options <- sort(unique(var_vals))
      
      updateCheckboxGroupInput(
        session, "var_on_plot",
        choices = var_options,
        selected = var_options
      )
    } else {
      updateCheckboxGroupInput(session, "var_on_plot", choices = character(0), selected = character(0))
    }
  })
  

  # Creating response over time plot - plot 1 in tab 1
  output$resp_over_time <- renderPlot({
    req(data_input(), input$response_var, input$color_var)
    
    gg <- ggplot(data_input(), aes_string(x = "yst", y = input$response_var))
    
    if (input$color_var == "None") {
      gg <- gg + geom_point(color = "black", alpha = 0.6)
    } else {
      gg <- gg + geom_point(aes_string(color = input$color_var), alpha = 0.6)
    }
    
    gg <- gg + labs(
      title = paste(input$response_var, "Over Time"),
      x = "Year",
      y = input$response_var,
      color = if (input$color_var == "None") NULL else input$color_var
    )
    
    gg
  })
  
  
  
  
  
  
  output$treat_minus_cntrl <- renderPlot({
    req(data_input())
    ggplot(data_input(), aes())
  })
  
  
  # UI for selecting custom x and y variables (for interactive plot)
  output$xvar_ui <- renderUI({
    req(data_input())
    updateSelectInput(session, "xvar", "X-axis variable:", choices = names(data_input()))
  })
  
  output$yvar_ui <- renderUI({
    req(data_input())
    selectInput("yvar", "Y-axis variable:", choices = names(data_input()))
  })
  
  # Creating dynamic plot - tab 1 plot 3
  output$custom_plot <- renderPlot({
    req(data_input(), input$xvar, input$yvar)
    ggplot(data_input(), aes_string(x = input$xvar_ui, y = input$yvar_ui)) +
      geom_point(alpha = 1) +
      labs(title = paste(input$yvar_ui, "vs", input$xvar_ui))
  })
  
  
  # Dynamic UI for study selection
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

