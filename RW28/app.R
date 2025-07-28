# Load required libraries
library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyverse)

# Labels used for plots
response_choices <- c("Height" = "ht",
                      "DBH" = "dbh",
                      "Volume" = "vol")

response_choices_plot2 <- c("Height" = "ht",
                            "DBH" = "dbh",
                            "Volume" = "vol",
                            "Total P" = "totp",
                            "Cum. P" = "cump")

color_choices <- c(
  "None" = "None",
  "Treatment Code" = "TRT_CODE",
  "Plot (factor)" = "plot_fac",
  "Study (factor)" = "stdy_fac",
  "Plot Bin (factor)" = "plot_bin"
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
  plot_fac = "Plot (factor)",
  plot_bin = "Plot Bins (facor)"
)

ui <- fluidPage(
  titlePanel("Model Builder: OLS vs GLM Gamma (log link) vs Linear Mixed Effects"),
  
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
                             selected = NULL)),
        
        tags$br(),
        tags$br(),
        tags$br(),
        
        helpText("Plot 2 Options:"),
        
        selectInput(inputId = "response_var2",
                    label = "Select response variable:", 
                    choices = response_choices_plot2,
                    selected = "ht"),
        
        selectInput("comparison_mode", "Comparison Mode:",
                    choices = c("Averaged",
                                "Individual"), selected = "Averaged"),
        
        helpText("Note: 'Averaged' means the treatment plots in each plot bin (i.e. the 1000s)
                 will be averaged together to make one line in this plot. 'Individual' means
                 each plot will individually be compared to its respective control."),
        
        checkboxGroupInput(
          inputId = "plot2_groups",
          label = "Select groups to show in Plot 2:",
          choices = NULL,  # will update dynamically
          selected = NULL
        ),
        
        
        
        tags$br(),
        tags$br(),
        tags$br(),
        
        helpText("Plot 3 Options:"),
        
        selectInput(inputId = "xvar",
                    label = "X-axis variable for Interactive Plot:",
                    choices = NULL),
        
        selectInput(inputId = "yvar",
                    label = "Y-axis variable for Interactive Plot:",
                    choices = NULL),
        
        selectInput("colorvar_plot3",
                    label = "Color points by:",
                    choices = NULL)
      ),
      
      conditionalPanel(
        condition = "input.main_tabs != 'Data Exploration' && input.main_tabs != 'Linear Mixed Effects'",
        uiOutput("stdy_ui"),
        uiOutput("yst_ui"),
        
        uiOutput("response_ui"),
        uiOutput("predictors_ui"),
        
        actionButton("run_models", "Run Models")
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == 'Linear Mixed Effects'",
        
        selectInput("lme_resp", "Select Response Variable:",
                    choices = c("ht", "dbh", "vol"),
                    selected = "ht"),
        
        checkboxGroupInput("lme_fixed", "Select Fixed Effect(s):",
                           choices = c("yst", "cump", "estabp", "totp"),
                           selected = c("yst")),
        
        radioButtons("lme_group", "Random intercept grouping variable(s):",
                           choices = list("stdy" = "stdy",
                                          "stdy/plot (nested)" = "nested"),
                           selected = "stdy"),
        
        conditionalPanel(
          condition = "input.lme_group == 'nested'",
          selectInput("lme_study_filter", "Select Study to Analyze:",
                      choices = NULL,  # updated in server
                      selected = NULL)
        ),
        
        checkboxGroupInput("lme_random_slope_vars", "Select random slope variable(s):",
                           choices = c("yst", "cump", "estabp", "totp"),
                           selected = NULL),
        
        actionButton("run_lme", "Run LME Model"),
        
        tags$div(
          style = "margin-top: 20px; padding: 10px; border: 1px solid #ccc; background-color: #f9f9f9; border-radius: 5px;",
          tags$strong("Note on Model Fitting:"),
          tags$p("Not all combinations of fixed and random effects may be supported by the data. If the model fails to fit:"),
          tags$ul(
            tags$li("Try reducing the number of random slopes."),
            tags$li("Try reducing the total number of fixed and random effects"),
            tags$li("Avoid selecting all predictors as both fixed and random effects."),
            tags$li("Ensure there are enough observations per group.")
          ),
          tags$p("This app gives you full control over model specification. If the model fails, check the error message to the right and try adjusting your inputs.")
        )
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
                 plotlyOutput("treat_minus_cntrl"),
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
        ),
        tabPanel("Linear Mixed Effects",
                 verbatimTextOutput("lme_summary"),
                 plotOutput("lme_effects_plot"),
                 plotOutput("lme_fit_obs_plot"))
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
        estabp = (as.numeric(totp) - as.numeric(cump)),
        TRT_CODE = substr(plot, 2, 4),
        stdy_fac = as.factor(stdy),
        plot_fac = as.factor(plot),
        plot_bin = ifelse(
          !is.na(plot),
          paste0(substr(as.character(plot), 1, 1), "000s"),
          NA_character_
        ) 
      )
    
    df
  })
  
  # Update Treatment Code / Study checkboxes dynamically for plot 1
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
  
  # Plot 1: response over time with robust color logic
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
  
  
  ##################################################################
  
  # Treatment - Control Plot 2
  # Plot 2 - Treatment minus Control
  output$treat_minus_cntrl <- renderPlotly({
    req(data_input(), input$response_var2, input$comparison_mode)
    
    df <- data_input()
    
    # Create plot_num and treatment_status
    df <- df %>%
      mutate(
        plot_num = as.numeric(gsub("[^0-9]", "", plot)),
        treatment_status = ifelse(plot_num %% 1000 == 0, "control", "treated"),
        plot_bin = paste0(substr(as.character(plot), 1, 1), "000s"),
        group_label = paste0(plot_bin, " (Study ", stdy, ")")
      )
    
    # Debug print: check group_label exists
    # print(head(df %>% select(plot, plot_num, treatment_status, group_label)))
    
    # Filter groups if selected
    if (!is.null(input$plot2_groups)) {
      df <- df %>% filter(group_label %in% input$plot2_groups)
    }
    
    response_var <- input$response_var2
    
    if (input$comparison_mode == "Averaged") {
      df_summary <- df %>%
        group_by(stdy, yst, plot_bin, treatment_status) %>%
        summarise(mean_val = mean(.data[[response_var]], na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = treatment_status, values_from = mean_val)
      
      # Ensure both treated and control columns exist
      if (!all(c("treated", "control") %in% names(df_summary))) {
        return(NULL)
      }
      
      df_summary <- df_summary %>%
        mutate(diff = treated - control,
               group_label = paste0(plot_bin, " (Study ", stdy, ")"))
      
      p <- ggplot(df_summary, aes(x = yst, y = diff, color = group_label, group = group_label)) +
        geom_line(size = 1) +
        labs(
          title = "Averaged Treatment - Control",
          x = "Year",
          y = paste("Difference in", label_lookup[[response_var]]),
          color = "Group (Plot Bin & Study)"
        )
      
    } else {
      # Individual mode
      
      df_control <- df %>%
        filter(treatment_status == "control") %>%
        select(stdy, yst, plot_bin, control_val = .data[[response_var]])
      
      df_treated <- df %>%
        filter(treatment_status == "treated") %>%
        select(stdy, yst, plot_bin, plot, treated_val = .data[[response_var]])
      
      df_joined <- df_treated %>%
        left_join(df_control, by = c("stdy", "yst", "plot_bin")) %>%
        mutate(diff = treated_val - control_val) %>%
        filter(!is.na(diff)) %>%
        mutate(group_label = paste0(plot_bin, " (Study ", stdy, ")"))
      
      # Debug print: check df_joined columns
      # print(head(df_joined))
      
      p <- ggplot(df_joined, aes(x = yst, y = diff, group = plot, color = group_label)) +
        geom_line(alpha = 0.7) +
        labs(
          title = "Individual Treatment - Control",
          x = "Year",
          y = paste("Difference in", label_lookup[[response_var]]),
          color = "Group (Plot Bin & Study)"
        )
    }
    
    ggplotly(p)
  })
  
  

  
  
  

  # dynamically change the checkbox options for plot 2
  observeEvent(data_input(), {
    df <- data_input()
    
    groups <- unique(paste0(df$plot_bin, " (Study ", df$stdy, ")"))
    groups <- sort(groups)
    
    updateCheckboxGroupInput(session, "plot2_groups",
                             choices = groups,
                             selected = groups)  # select all by default
  })
  
  
  ##################################################################
  
  # Updates available X and Y variables for interactive plot - plot 3
  observeEvent(data_input(), {
    vars <- names(data_input())
    updateSelectInput(session, "xvar", choices = vars, selected = vars[1])
    updateSelectInput(session, "yvar", choices = vars, selected = vars[2])
    updateSelectInput(session, "colorvar_plot3", choices = c("None", vars), selected = "None")
    
  })
  
  # Plot 3: Custom interactive plot
  output$custom_plot <- renderPlot({
    req(data_input(), input$xvar, input$yvar)
    
    df <- data_input()
    
    x_label <- label_lookup[[input$xvar]]
    if (is.null(x_label)) x_label <- input$xvar
    
    y_label <- label_lookup[[input$yvar]]
    if (is.null(y_label)) y_label <- input$yvar
    
    color_label <- if (input$colorvar_plot3 == "None") NULL else input$colorvar_plot3
    
    p <- ggplot(df, aes_string(x = input$xvar, y = input$yvar))
    
    if (!is.null(color_label) && color_label != "None") {
      p <- p + geom_point(aes_string(color = color_label), alpha = 0.8)
    } else {
      p <- p + geom_point(alpha = 0.8)
    }
    
    p + labs(
      title = paste(y_label, "vs", x_label),
      x = x_label,
      y = y_label,
      color = color_label
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
  
  #Linear Mixed Effects tab
  
  observe({
    df <- data_input()
    updateSelectInput(inputId = "lme_study_filter",
                      choices = sort(unique(df$stdy)),
                      selected = unique(df$stdy)[1])
  })
  
  observeEvent(input$run_lme, {
    req(input$lme_fixed, input$lme_group, input$lme_resp)
    
    # Get data
    df <- data_input() %>%
      select(all_of(c(input$lme_resp, "stdy", "plot", "yst", "cump", "estabp", "totp"))) %>%
      na.omit()
    
    # Handle filtering based on nested grouping or slope choices
    if (input$lme_group == "nested") {
      req(input$lme_study_filter)
      df <- df %>% filter(stdy == input$lme_study_filter)
    }
      
      # Already filtered df above, so just check data counts here
      n_obs <- nrow(df)
      n_plot <- length(unique(df$plot))
      
      if (n_obs <= n_plot) {
        showNotification("Not enough data per plot to fit the model. Try a different study.", type = "error")
        return(NULL)
      }
    
    # Build fixed effects formula
    fixed_formula <- paste(input$lme_resp, "~", paste(input$lme_fixed, collapse = " + "))
    
    
    # Build random effects formula
    if (input$lme_group == "nested") {
      # Nested structure: plot within study (stdy/plot)
      
      if (length(input$lme_random_slope_vars) > 0) {
        # Random slope(s) for plot (within selected study)
        random_formula <- as.formula(
          paste0("~ ", paste(input$lme_random_slope_vars, collapse = " + "), " | plot")
        )
      } else {
        # Random intercept for plot within a single study
        random_formula <- as.formula("~ 1 | plot")
      }
      
    } else if (input$lme_group == "stdy") {
      # Grouping by study only
      
      if (length(input$lme_random_slope_vars) > 0) {
        # Random slopes for selected variables by study
        random_formula <- as.formula(
          paste0("~ ", paste(input$lme_random_slope_vars, collapse = " + "), " | stdy")
        )
      } else {
        # Random intercept only for study
        random_formula <- as.formula("~ 1 | stdy")
      }
    }
    
    # Fit the model
    model <- tryCatch({
      nlme::lme(fixed = as.formula(fixed_formula),
                random = random_formula,
                data = df,
                na.action = na.omit, method = "ML")
    }, error = function(e) e)
    
    output$lme_summary <- renderPrint({
      if (inherits(model, "error")) {
        cat("Model failed to fit:\n")
        print(model$message)
      } else {
        summary(model)
      }
    })
    
    #Fixed + Random Effects Plot
    output$lme_effects_plot <- renderPlot({
      if (!inherits(model, "error")) {
        plot(nlme::ranef(model), main = "Random Effects Estimates")
      }
    })
    
    #Fitted vs Observed plot
    output$lme_fit_obs_plot <- renderPlot({
      if (!inherits(model, "error")) {
        ggplot(data = data.frame(Fitted = fitted(model),
                                 Observed = df[[input$lme_resp]]),
               aes(x = Fitted, y = Observed)) +
          geom_point(alpha = 0.6) +
          geom_abline(slope = 1, intercept = 0, color = "red") +
          theme_minimal() +
          labs(title = "Fitted vs Observed", x = "Fitted Values", y = "Observed Values")
      }
    })
    
  })
  
  
}

shinyApp(ui, server)

