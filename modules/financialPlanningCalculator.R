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
        status = "white",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        fluidRow(
          column(width = 6,
                 h4("Accumulation Phase Inputs"),
                 autonumericInput(inputId = ns("initial"),  label = "Initial Investment (KES):",  value = 1000000, decimalPlaces = 0, digitGroupSeparator = ","),
                 autonumericInput(inputId = ns("annual"),  label = "Annual Contribution (KES):",  value = 500000, decimalPlaces = 0, digitGroupSeparator = ","),
                 numericInput(ns("accYears"), "Years to Retirement:", value = 30, min = 1, step = 1),
                 numericInput(ns("accRate"), "Annual Return Rate (%):", value = 10, min = 0, step = 0.1),
                 numericInput(ns("accInflation"), "Annual Inflation Rate (%):", value = 5, min = 0, step = 0.1)
          ),
          column(width = 6,
                 h4("Withdrawal Phase Inputs"),
                 autonumericInput(inputId = ns("retPortfolio"),  label = "Retirement Portfolio (KES):",  value = 5000000, decimalPlaces = 0, digitGroupSeparator = ","),
                 autonumericInput(inputId = ns("withdrawal"),  label = "Initial Annual Withdrawal (KES):",  value = 400000, decimalPlaces = 0, digitGroupSeparator = ","),
                 numericInput(ns("retYears"), "Years in Retirement:", value = 25, min = 1, step = 1),
                 numericInput(ns("retRate"), "Annual Return Rate in Retirement (%):", value = 8, min = 0, step = 0.1),
                 numericInput(ns("retInflation"), "Annual Inflation Rate (%):", value = 5, min = 0, step = 0.1)
          )
        ),
        fluidRow(
          column(width = 12, align = "center",
                 actionButton(ns("update"), "Update Projections", class = "btn-primary control-button")
          )
        )
      )
    ),
    # Output Cards: Accumulation and Withdrawal projections
    fluidRow(
      column(width = 6,
             bs4Card(
               title = "Accumulation Phase",
               status = "white",
               solidHeader = TRUE,
               width = 12,
               tabsetPanel(
                 tabPanel("Plot", plotlyOutput(ns("accPlot"), height = "400px")),
                 tabPanel("Data Table", tableOutput(ns("accTable")))
               )
             )
      ),
      column(width = 6,
             bs4Card(
               title = "Withdrawal Phase",
               status = "white",
               solidHeader = TRUE,
               width = 12,
               tabsetPanel(
                 tabPanel("Plot", plotlyOutput(ns("retPlot"), height = "400px")),
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
    

    output$accPlot <- renderPlotly({
      df <- accData()
      # Create Plotly plot with two lines for nominal and real values
      plot_ly(data = df) %>%
        add_lines(x = ~Year, y = ~Nominal, 
                  line = list(color = "blue", width = 2),
                  name = "Nominal Value (KES)") %>%
        add_lines(x = ~Year, y = ~Real, 
                  line = list(color = "red", width = 2, dash = "dash"),
                  name = "Real Value (KES, Adjusted)") %>%
        layout(title = list(text = "Accumulation Phase Projection"),
              xaxis = list(title = "Year"),
              yaxis = list(title = "Portfolio Value (KES)", 
                            tickformat = ",.0f"),  # Format y-axis with commas for thousands
              legend = list(title = list(text = "Legend")),
              margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
              font = list(size = 12))
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
    
    output$retPlot <- renderPlotly({
      df <- retData()
      # Create Plotly plot with two lines for portfolio balance and annual withdrawal
      plot_ly(data = df) %>%
        add_lines(x = ~Year, y = ~Portfolio, 
                  line = list(color = "green", width = 2),
                  name = "Portfolio Balance (KES)") %>%
        add_lines(x = ~Year, y = ~Withdrawal, 
                  line = list(color = "purple", width = 2, dash = "dash"),
                  name = "Annual Withdrawal (KES)") %>%
        layout(title = list(text = "Withdrawal Phase Projection"),
              xaxis = list(title = "Year"),
              yaxis = list(title = "Amount (KES)", 
                            tickformat = ",.0f"),  # Format y-axis with commas for thousands
              legend = list(title = list(text = "Legend")),
              margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
              font = list(size = 12))
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
