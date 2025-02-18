# financialPlanningCalculator Module

# Module UI for the Financial Planning Calculator
financialPlanningCalcUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        div(
          h2("Financial Planning Calculator", class = "page-title")
        )
      )
    ),
    # Input Parameters in a collapsible card
    fluidRow(
      bs4Card(
        title = "Input Parameters",
        status = "primary",
        width = 12,
        collapsible = TRUE,
        fluidRow(
          column(width = 6,
                 h4("Accumulation Phase Inputs"),
                 numericInput(ns("initial"), "Initial Investment (KES):", value = 1000000, min = 0, step = 10000),
                 numericInput(ns("annual"), "Annual Contribution (KES):", value = 500000, min = 0, step = 10000),
                 numericInput(ns("accYears"), "Years to Retirement:", value = 30, min = 1, step = 1),
                 numericInput(ns("accRate"), "Annual Return Rate (%):", value = 10, min = 0, step = 0.1),
                 numericInput(ns("accInflation"), "Annual Inflation Rate (%):", value = 5, min = 0, step = 0.1)
          ),
          column(width = 6,
                 h4("Withdrawal Phase Inputs"),
                 numericInput(ns("retPortfolio"), "Retirement Portfolio (KES):", value = 5000000, min = 0, step = 10000),
                 numericInput(ns("withdrawal"), "Initial Annual Withdrawal (KES):", value = 400000, min = 0, step = 10000),
                 numericInput(ns("retYears"), "Years in Retirement:", value = 25, min = 1, step = 1),
                 numericInput(ns("retRate"), "Annual Return Rate in Retirement (%):", value = 8, min = 0, step = 0.1),
                 numericInput(ns("retInflation"), "Annual Inflation Rate (%):", value = 5, min = 0, step = 0.1)
          )
        ),
        fluidRow(
          column(width = 12, align = "center",
                 actionButton(ns("update"), "Update Projections", class = "btn-primary")
          )
        )
      )
    ),
    # Output Cards: Accumulation and Withdrawal projections
    fluidRow(
      column(width = 6,
             bs4Card(
               title = "Accumulation Phase",
               status = "info",
               width = 12,
               tabsetPanel(
                 tabPanel("Plot", plotOutput(ns("accPlot"), height = "400px")),
                 tabPanel("Data Table", tableOutput(ns("accTable")))
               )
             )
      ),
      column(width = 6,
             bs4Card(
               title = "Withdrawal Phase",
               status = "info",
               width = 12,
               tabsetPanel(
                 tabPanel("Plot", plotOutput(ns("retPlot"), height = "400px")),
                 tabPanel("Data Table", tableOutput(ns("retTable")))
               )
             )
      )
    )
  )
}

# Module Server for the Financial Planning Calculator
financialPlanningCalcServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    comma_format <- function(accuracy = 1) {
        function(x) {
            # Round the value based on the given accuracy
            x <- round(x, digits = accuracy)
            # Format the numbers with commas as thousand separators
            format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
        }
        }

    ### Accumulation Phase Calculations ###
    accData <- eventReactive(input$update, {
      years <- 0:input$accYears
      nominal <- numeric(length(years))
      real <- numeric(length(years))
      
      # Set starting values
      nominal[1] <- input$initial
      real[1] <- input$initial
      
      # Convert rates from percentage to decimal
      r <- input$accRate / 100
      inflation <- input$accInflation / 100
      contribution <- input$annual
      
      # Calculate portfolio values over time
      for (i in 2:length(years)) {
        nominal[i] <- nominal[i - 1] * (1 + r) + contribution
        real[i] <- nominal[i] / ((1 + inflation)^(years[i]))
      }
      
      data.frame(Year = years, Nominal = nominal, Real = real)
    }, ignoreNULL = FALSE)
    
    output$accPlot <- renderPlot({
      df <- accData()
      ggplot(df, aes(x = Year)) +
        geom_line(aes(y = Nominal, color = "Nominal Value (KES)"), size = 1.2) +
        geom_line(aes(y = Real, color = "Real Value (KES, Adjusted)"), size = 1.2, linetype = "dashed") +
        scale_y_continuous(labels = comma_format(accuracy = 0)) +
        labs(title = "Accumulation Phase Projection", y = "Portfolio Value (KES)", color = "Legend") +
        theme_minimal()
    })
    
    output$accTable <- renderTable({
      df <- accData()
      data.frame(
        Year = df$Year, 
        Nominal = format(df$Nominal, big.mark = ",", scientific = FALSE, trim = TRUE, digits = 1, nsmall = 0),
        Real = format(df$Real, big.mark = ",", scientific = FALSE, trim = TRUE, digits = 1, nsmall = 0)
      )
    })
    
    ### Withdrawal Phase Calculations ###
    retData <- eventReactive(input$update, {
      years <- 0:input$retYears
      portfolio <- numeric(length(years))
      withdrawal <- numeric(length(years))
      
      portfolio[1] <- input$retPortfolio
      withdrawal[1] <- input$withdrawal
      
      r <- input$retRate / 100
      inflation <- input$retInflation / 100
      
      for (i in 2:length(years)) {
        withdrawal[i] <- withdrawal[i - 1] * (1 + inflation)
        portfolio[i] <- portfolio[i - 1] * (1 + r) - withdrawal[i - 1]
      }
      
      data.frame(Year = years, Portfolio = portfolio, Withdrawal = withdrawal)
    }, ignoreNULL = FALSE)
    
    output$retPlot <- renderPlot({
      df <- retData()
      ggplot(df, aes(x = Year)) +
        geom_line(aes(y = Portfolio, color = "Portfolio Balance (KES)"), size = 1.2) +
        geom_line(aes(y = Withdrawal, color = "Annual Withdrawal (KES)"), size = 1.2, linetype = "dashed") +
        scale_y_continuous(labels = comma_format(accuracy = 0)) +
        labs(title = "Withdrawal Phase Projection", y = "Amount (KES)", color = "Legend") +
        theme_minimal()
    })
    
    output$retTable <- renderTable({
      df <- retData()
      data.frame(
        Year = df$Year, 
        Portfolio = format(df$Portfolio, big.mark = ",", scientific = FALSE, trim = TRUE, digits = 1, nsmall = 0),
        Withdrawal = format(df$Withdrawal, big.mark = ",", scientific = FALSE, trim = TRUE, digits = 1, nsmall = 0)
      )
    })
    
  })
}
