library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("OLS Model Builder from Excel Data"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File (.xlsx)", accept = ".xlsx"),
      textInput("sheet", "Sheet Name", value = "plot data"),
      
      uiOutput("stdy_ui"),
      uiOutput("yst_ui"),
      
      uiOutput("response_ui"),
      uiOutput("predictors_ui"),
      actionButton("run_model", "Run OLS Model")
    ),
    
    mainPanel(
      verbatimTextOutput("model_summary"),
      plotOutput("diagnostic_plots")
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
  
  # Response variable selection
  output$response_ui <- renderUI({
    req(filtered_data())
    selectInput("response", "Select Response Variable", choices = names(filtered_data()), selected = "ht")
  })
  
  # Predictor variable selection
  output$predictors_ui <- renderUI({
    req(filtered_data(), input$response)
    choices <- setdiff(names(filtered_data()), c("stdy", "plot", "yst", input$response))
    checkboxGroupInput("predictors", "Select Predictor Variables", choices = choices)
  })
  
  # Fit OLS model
  model_result <- eventReactive(input$run_model, {
    req(input$response, input$predictors)
    formula_text <- paste(input$response, "~", paste(input$predictors, collapse = " + "))
    lm(as.formula(formula_text), data = filtered_data())
  })
  
  # Display model summary
  output$model_summary <- renderPrint({
    req(model_result())
    summary(model_result())
  })
  
  # Diagnostic plots
  output$diagnostic_plots <- renderPlot({
    req(model_result())
    par(mfrow = c(2, 2))
    plot(model_result())
  })
}

shinyApp(ui, server)

