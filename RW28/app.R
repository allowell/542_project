# Load required libraries
library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)

# Labels used for plots
response_choices <- c("Height" = "ht",
                      "DBH" = "dbh",
                      "Volume" = "vol")

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
  cump = "Cumulative Phosphorus",
  totp = "Total Phosphorus",
  estabp = "Established Phosphorus",
  yst = "Year",
  stdy = "Study",
  plot = "Plot",
  TRT_CODE = "Treatment Code",
  stdy_fac = "Study (factor)",
  plot_fac = "Plot (factor)"
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
      
      tags$br(),
      tags$br(),
      
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
                             selected = NULL)
        ),
        
        tags$br(),
        tags$br(),
        tags$br(),
        
        helpText("Plot 2 Options:"),
        
        selectInput(inputId = "response_var2",
                    label = "Select response variable:", 
                    choices = response_choices,
                    selected = "None"),
        
        tags$br(),
        tags$br(),
        tags$br(),
        
        helpText("Plot 3 Options:"),
        
        selectInput(inputId = "xvar",
                    label = "X-axis variable for Interactive Plot:",
                    choices = NULL),
        
        selectInput(inputId = "yvar",
                    label = "Y-axis variable for Interactive Plot:",
                    choices = NULL)
      ),
      
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
                 plotlyOutput("resp_over_time"),
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
  
  # Update Treatment Code / Study checkboxes dynamically
  observeEvent({
    input$color_var
    data_input()
  }, {
    req(data_input(), input$color_var)
    
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
  
  # ✅ Plot 1: response over time with robust color logic
  output$resp_over_time <- renderPlotly({
    req(data_input(), input$response_var)
    
    df <- data_input()
    
    # Filter by Treatment Code or Study selection if applicable
    if (input$color_var != "None") {
      req(input$var_on_plot)
      df <- df %>% filter(.data[[input$color_var]] %in% input$var_on_plot)
    }
    
    response_label <- names(response_choices)[response_choices == input$response_var]
    color_label <- if (input$color_var == "None") NULL else names(color_choices)[color_choices == input$color_var]
    
    # Plot with or without color mapping
    if (input$color_var == "None") {
      gg <- ggplot(df, aes_string(
        x = "yst",
        y = input$response_var,
        group = "plot",
        text = "stdy"
      )) +
        geom_line(alpha = 0.5, color = "black") +
        geom_point(alpha = 0.8, color = "black") +
        labs(
          title = paste(response_label, "Over Time"),
          x = "Year",
          y = response_label
        )
    } else {
      gg <- ggplot(df, aes_string(
        x = "yst",
        y = input$response_var,
        group = "plot",
        color = input$color_var,
        text = "stdy"
      )) +
        geom_line(alpha = 0.5) +
        geom_point(alpha = 0.8) +
        labs(
          title = paste(response_label, "Over Time"),
          x = "Year",
          y = response_label,
          color = color_label
        )
    }
    
    ggplotly(gg, tooltip = c("x", "y", "color", "text"))
  })
  
  # Plot 2 placeholder
  output$treat_minus_cntrl <- renderPlot({
    req(data_input())
    ggplot(data_input(), aes())
  })
  
  # Update X and Y variable selections for Plot 3
  observeEvent(data_input(), {
    vars <- names(data_input())
    updateSelectInput(session, "xvar", choices = vars, selected = vars[1])
    updateSelectInput(session, "yvar", choices = vars, selected = vars[2])
  })
  
  # Plot 3: Custom interactive plot
  output$custom_plot <- renderPlot({
    req(data_input(), input$xvar, input$yvar)
    
    x_label <- label_lookup[[input$xvar]]
    if (is.null(x_label)) x_label <- input$xvar
    
    y_label <- label_lookup[[input$yvar]]
    if (is.null(y_label)) y_label <- input$yvar
    
    ggplot(data_input(), aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(alpha = 1) +
      labs(
        title = paste(y_label, "vs", x_label),
        x = x_label,
        y = y_label
      )
  })
  
  # Dynamic UI for model tabs
  output$stdy_ui <- renderUI({
    req(data_input())
    selectInput("stdy_filter", "Select Study (stdy)", choices = unique(data_input()$stdy))
  })
  
  output$yst_ui <- renderUI({
    req(input$stdy_filter)
    df <- data_input() |> filter(stdy == input$stdy_filter)
    selectInput("yst_filter", "Select Year (yst)", choices = unique(df$yst))
  })
  
  filtered_data <- reactive({
    req(input$stdy_filter, input$yst_filter)
    data_input() |> filter(stdy == input$stdy_filter, yst == input$yst_filter)
  })
  
  output$response_ui <- renderUI({
    req(filtered_data())
    selectInput("response", "Select Response Variable", 
                choices = c("dbh", "ht", "vol"), 
                selected = "ht")
  })
  
  output$predictors_ui <- renderUI({
    req(filtered_data(), input$response)
    choices <- c("stdy", "plot", "yst", "cump", "totp", "estabp")
    checkboxGroupInput("predictors", "Select Predictor Variables", choices = choices)
  })
  
  models_result <- eventReactive(input$run_models, {
    req(input$response, input$predictors)
    
    formula_text <- paste(input$response, "~", paste(input$predictors, collapse = " + "))
    
    df <- filtered_data()
    
    model_ols <- lm(as.formula(formula_text), data = df)
    model_gamma <- glm(as.formula(formula_text), data = df, family = Gamma(link = "log"))
    
    list(ols = model_ols, gamma = model_gamma)
  })
  
  output$model_summary_ols <- renderPrint({
    req(models_result())
    summary(models_result()$ols)
  })
  
  output$diagnostic_plots_ols <- renderPlot({
    req(models_result())
    par(mfrow = c(2, 2))
    plot(models_result()$ols)
  })
  
  output$model_summary_gamma <- renderPrint({
    req(models_result())
    summary(models_result()$gamma)
  })
  
  output$diagnostic_plots_gamma <- renderPlot({
    req(models_result())
    par(mfrow = c(2, 2))
    plot(models_result()$gamma)
  })
  
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

