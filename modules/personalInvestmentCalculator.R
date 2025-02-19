# personalInvestmentCalculator Module
# Module UI: Defines the layout for inputs and outputs
personalInvestmentCalcUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        div(
          h2("Personal Investment Calculator", class = "page-title")
        )
      )
    ),
    fluidRow(
      box(
        title = "Investment Inputs", status = "white", solidHeader = TRUE, width = 4,
        numericInput(ns("initial"), "Initial Investment (KES):", value = 100000, min = 0, step = 1000),
        numericInput(ns("contribution"), "Monthly Contribution (KES):", value = 10000, min = 0, step = 1000),
        numericInput(ns("rate"), "Annual Interest Rate (%):", value = 8, min = 0, step = 0.1),
        numericInput(ns("years"), "Investment Duration (years):", value = 10, min = 1, step = 1),
        actionButton(ns("calculate"), "Calculate", class = "btn-success control-button") 
      ),
      box(
        title = "Results", status = "white", solidHeader = TRUE, width = 8,
        plotlyOutput(ns("growthPlot"))
      )
    ),
    fluidRow(
      box(
        title = "Summary Table", status = "white", solidHeader = TRUE, width = 12,
        dataTableOutput(ns("summaryTable"))
      )
    )
  )
}

# Module Server: Contains the reactive calculations and output renderings
personalInvestmentCalcServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
 
  calculate_investment <- eventReactive(input$calculate, {
    initial <- input$initial
    monthly <- input$contribution
    rate <- input$rate / 100 / 12
    months <- input$years * 12
    
    balance <- numeric(months)
    balance[1] <- initial * (1 + rate)
    
    for (i in 2:months) {
      balance[i] <- (balance[i-1] + monthly) * (1 + rate)
    }
    
    data.frame(Month = 1:months, Balance = balance) %>%
      mutate(Balance = paste0("KES ", formatC(Balance, format = "f", big.mark = ",", digits = 0)))
  })

  output$growthPlot <- renderPlotly({
  df <- calculate_investment()
  # Clean up the Balance column
  df$Balance <- as.numeric(gsub("KES ", "", gsub(",", "", df$Balance)))
  # Create the Plotly plot
  plot_ly(data = df, 
          x = ~Month, 
          y = ~Balance,
          type = 'scatter', 
          mode = 'lines',
          line = list(color = 'blue', width = 2)) %>%
    layout(title = list(text = "Investment Growth Over Time"),
           xaxis = list(title = "Months"),
           yaxis = list(title = "Balance (KES)"),
           font = list(size = 12),
           margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))
})
  
  output$summaryTable <- renderDataTable({
    df <- calculate_investment()
    datatable(df, options = list(pageLength = 10))
  })

  })
}
