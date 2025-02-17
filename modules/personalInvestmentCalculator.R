# personalInvestmentCalculator Module
# Module UI: Defines the layout for inputs and outputs
personalInvestmentCalcUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Investment Inputs", status = "primary", solidHeader = TRUE, width = 4,
        numericInput(ns("initial"), "Initial Investment (KES):", value = 100000, min = 0, step = 1000),
        numericInput(ns("contribution"), "Monthly Contribution (KES):", value = 10000, min = 0, step = 1000),
        numericInput(ns("rate"), "Annual Interest Rate (%):", value = 8, min = 0, step = 0.1),
        numericInput(ns("years"), "Investment Duration (years):", value = 10, min = 1, step = 1),
        actionButton(ns("calculate"), "Calculate", class = "btn-success")
      ),
      box(
        title = "Results", status = "success", solidHeader = TRUE, width = 8,
        plotOutput(ns("growthPlot"))
      )
    ),
    fluidRow(
      box(
        title = "Summary Table", status = "success", solidHeader = TRUE, width = 12,
        dataTableOutput(ns("summaryTable"))
      )
    )
  )
}

# Module Server: Contains the reactive calculations and output renderings
personalInvestmentCalcServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Calculate investment growth when the "Calculate" button is pressed
    calcInvestment <- eventReactive(input$calculate, {
      initial <- input$initial
      monthly <- input$contribution
      rate <- input$rate / 100 / 12
      months <- input$years * 12
      
      balance <- numeric(months)
      balance[1] <- initial * (1 + rate)
      
      for (i in 2:months) {
        balance[i] <- (balance[i - 1] + monthly) * (1 + rate)
      }
      
      # Create raw data frame with numeric balance
      df <- data.frame(Month = 1:months, Balance = balance)
      df
    }, ignoreNULL = FALSE)
    
    # Plot: Use the raw balance for plotting
    output$growthPlot <- renderPlot({
      df <- calcInvestment()
      ggplot(df, aes(x = Month, y = Balance)) +
        geom_line(color = "blue", size = 1) +
        labs(title = "Investment Growth Over Time", x = "Months", y = "Balance (KES)") +
        scale_y_continuous(labels = comma_format(accuracy = 1)) +
        theme_minimal()
    })
    
    # Summary Table: Format the balance for display with commas and no decimals
    output$summaryTable <- renderDataTable({
      df <- calcInvestment()
      df <- df %>%
        mutate(Balance = paste0("KES ", formatC(Balance, format = "f", big.mark = ",", digits = 0)))
      datatable(df, options = list(pageLength = 10))
    })
  })
}
