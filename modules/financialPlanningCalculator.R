# financialPlanningCalculator Module

# Define goal settings as a named list.
goalSettings <- list(
  "Education" = list(minAmount = 1000000, defaultTerm = 10, defaultAmount = 5000000),
  "Building a House" = list(minAmount = 1000000, defaultTerm = 5, defaultAmount = 10000000),
  "Retirement" = list(minAmount = 500000, defaultTerm = 10, defaultAmount = 5000000),
  "Business/Start Up" = list(minAmount = 50000, defaultTerm = 3, defaultAmount = 1000000),
  "Emergency Fund" = list(minAmount = 50000, defaultTerm = 10, defaultAmount = 1000000),
  "Wedding/Dowry/Bride Price" = list(minAmount = 50000, defaultTerm = 1, defaultAmount = 500000),
  "Vehicle Purchase" = list(minAmount = 250000, defaultTerm = 3, defaultAmount = 500000),
  "Travel/Vacation" = list(minAmount = 50000, defaultTerm = 1, defaultAmount = 250000),
  "HealthCare Buffer" = list(minAmount = 50000, defaultTerm = 1, defaultAmount = 250000),
  "Graduation/Social Event" = list(minAmount = 50000, defaultTerm = 0.25, defaultAmount = 50000),
  "Personal Purchase e.g. Phone, Laptop" = list(minAmount = 50000, defaultTerm = 0.5, defaultAmount = 50000),
  "Investment" = list(minAmount = 100000, defaultTerm = 3, defaultAmount = 1000000),
  "Other" = list(minAmount = 50000, defaultTerm = 0.25, defaultAmount = 50000)
)

# Module UI for the Financial Planning Calculator
financialPlanningCalcUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        div(
          h2("Financial Planning Calculator", class = "page-title"),
          p("A Financial Planning Calculator is a comprehensive tool designed to help individuals create a roadmap for achieving their financial goals. It considers various aspects of personal finance, including savings, investments, expenses, debt, and retirement planning.",
            style = "margin-top: 10px;")
        )
      )
    ),
    # Input Parameters in a collapsible card
    fluidRow(
      bs4Card(
        title = "Personal Financial Profile",
        status = "secondary",
        width = 6,
        collapsible = TRUE,
        bs4Dash::tooltip(
          selectInput(ns("goal"), "Financial Goal:",
                      choices = names(goalSettings),
                      selected = "Building a House"),
          title = "Select your financial goal. For each goal, default minimum amounts and terms are defined.",
          placement = "right"
        ),
        # Annual Income
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("income"), 
                           label = "Annual Income (USD):",  
                           value = 80000, 
                           decimalPlaces = 0, 
                           digitGroupSeparator = ","),
          title = "Enter your current annual income in USD.",
          placement = "right"
        ),
        # Monthly Expenses
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("expenses"), 
                           label = "Monthly Expenses (USD):",  
                           value = 3000, 
                           decimalPlaces = 0, 
                           digitGroupSeparator = ","),
          title = "Enter your average monthly expenses in USD.",
          placement = "right"
        ),
        # Current Savings
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("savings"), 
                           label = "Current Savings (USD):",  
                           value = 20000, 
                           decimalPlaces = 0, 
                           digitGroupSeparator = ","),
          title = "Enter the total amount you currently have saved.",
          placement = "right"
        ),
        # Total Debt
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("debt"), 
                           label = "Total Debt (USD):",  
                           value = 30000, 
                           decimalPlaces = 0, 
                           digitGroupSeparator = ","),
          title = "Enter your current total debt.",
          placement = "right"
        ),
         # Investment Portfolio
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("portfolio"), 
                           label = "Investment Portfolio (USD):",  
                           value = 50000, 
                           decimalPlaces = 0, 
                           digitGroupSeparator = ","),
          title = "Enter the total value of your investment portfolio.",
          placement = "right"
        )),
        bs4Card(
        title = "Goal Settings & Economic Assumptions",
        status = "secondary",
        width = 6,
        collapsible = TRUE,
        # Emergency Fund
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("emergency"), 
                           label = "Emergency Fund (USD):",  
                           value = 15000, 
                           decimalPlaces = 0, 
                           digitGroupSeparator = ","),
          title = "Enter the amount set aside for emergencies.",
          placement = "right"
        ),
         # Effective Tax Rate
        bs4Dash::tooltip(
          numericInput(ns("tax_rate"), "Effective Tax Rate (%):", value = 20, min = 0, step = 0.1),
          title = "Enter your effective tax rate (as a percentage).",
          placement = "right"
        ),
         # Goal Amount (auto defaults based on selected goal if desired)
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("goal_amount"), 
                           label = "Goal Amount (USD):",  
                           value = 50000, 
                           decimalPlaces = 0, 
                           digitGroupSeparator = ","),
          title = "Enter the target amount you want to achieve for your selected goal.",
          placement = "right"
        ),
         # Goal Term in years
        bs4Dash::tooltip(
          numericInput(ns("goal_term"), "Goal Term (years):", value = 5, min = 0.25, step = 0.25),
          title = "Enter the number of years by which you want to achieve your goal.",
          placement = "right"
        ),
         # Expected Rate of Return
        bs4Dash::tooltip(
          numericInput(ns("exp_return"), "Expected Annual Return (%):", value = 7, min = 0, step = 0.1),
          title = "Enter the expected annual rate of return on your investments.",
          placement = "right"
        ),
         # Annual Inflation Rate
        bs4Dash::tooltip(
          numericInput(ns("inflation_rate"), "Annual Inflation Rate (%):", value = 2, min = 0, step = 0.1),
          title = "Enter the expected annual inflation rate.",
          placement = "right"
        )                          
      )),
         # Row 3: Calculate button
      fluidRow(
        column(
          width = 12,
          align = "center",
                  # Generate Projections Button
        actionButton(ns("update"), "Generate Projections", class = "btn-primary control-button", style = "margin-bottom: 15px;") 
        )
      ),
     fluidRow(
      bs4Card(
        title = "Results Summary", status = "secondary", width = 12,
        fluidRow(
          div(style = "margin-bottom: 10px;", uiOutput(ns("fp_summary")))
        ),
        fluidRow(
          downloadButton(ns("download_excel"), "Download Schedule (Excel)", class = "btn-info contr")
        )
      )
    ),
    # Graphs: Placed below the inputs/results
    fluidRow(
      bs4Card(
        title = "Investment Projection - Nominal", status = "secondary", width = 12,
        plotlyOutput(ns("nominalPlot"), height = "400px")
      )
    ),
    fluidRow(
      bs4Card(
        title = "Investment Projection - Inflation Adjusted", status = "secondary", width = 12,
        plotlyOutput(ns("realPlot"), height = "400px")
      )
    ),
    fluidRow(
      bs4Card(
        title = "Projection Schedule", status = "secondary", width = 12,
        dataTableOutput(ns("scheduleTable"))
      )
    )
  )
}


# Module Server for the Financial Planning Calculator
financialPlanningCalcServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive: Financial Profile Calculation triggered by "update" button
    fpData <- eventReactive(input$update, {
      # Net Worth calculation
      net_worth <- (input$savings + input$portfolio) - input$debt
      
      # Annual Savings: Assume savings come from (Income - (Expenses * 12))
      annual_savings <- input$income - (input$expenses * 12)
      
      # Future Value of Investments using compound interest
      total_principal <- input$savings + input$portfolio
      r <- input$exp_return / 100
      n <- input$goal_term  # number of years until goal
      fv_nominal <- total_principal * (1 + r)^n + annual_savings * (((1 + r)^n - 1) / r)
      
      # Adjust for inflation
      real_rate <- (input$exp_return - input$inflation_rate) / 100
      fv_real <- total_principal * (1 + real_rate)^n + annual_savings * (((1 + real_rate)^n - 1) / real_rate)
      
      # Gap for the Goal
      gap <- max(input$goal_amount - fv_nominal, 0)
      req_monthly <- if(gap > 0) gap / (n * 12) else 0
      
      list(net_worth = net_worth,
           annual_savings = annual_savings,
           fv_nominal = fv_nominal,
           fv_real = fv_real,
           gap = gap,
           req_monthly = req_monthly)
    }, ignoreInit = FALSE, ignoreNULL = FALSE)
    
    # Summary text output
    output$fp_summary <- renderUI({
      data <- fpData()
      
      additional_text <- if (data$gap > 0) {
        paste0(
          "<div style='margin-bottom:30px; font-size:16px;'><b>Additional Required Monthly Savings:</b> ", 
          scales::dollar(round(data$req_monthly, 0)), 
          "</div>"
        )
      } else {
        "<div style='margin-bottom:30px; font-size:16px;'>Your projected savings meet your goal!</div>"
      }
      
      HTML(paste0(
        "<div style='margin-bottom:20px; font-size:16px;'><b>Net Worth:</b> ", 
          scales::dollar(data$net_worth), 
        "</div>",
        "<div style='margin-bottom:20px; font-size:16px;'><b>Future Value (Nominal):</b> ", 
          scales::dollar(round(data$fv_nominal, 0)), 
        "</div>",
        "<div style='margin-bottom:20px; font-size:16px;'><b>Inflation-Adjusted Value:</b> ", 
          scales::dollar(round(data$fv_real, 0)), 
        "</div>",
        additional_text
      ))
    })

    
    # Schedule Data: Yearly projection for the accumulation phase
    scheduleData <- eventReactive(input$update, {
      n_years <- input$goal_term
      years <- 0:n_years
      r <- input$exp_return / 100
      annual_savings <- input$income - (input$expenses * 12)
      total_principal <- input$savings + input$portfolio
      nominal <- total_principal * (1 + r)^years + annual_savings * (((1 + r)^years - 1) / r)
      inflation <- input$inflation_rate / 100
      real <- nominal / ((1 + inflation)^years)
      data.frame(Year = years, Nominal = nominal, Real = real)
    })
    
    output$scheduleTable <- renderDataTable({
      df <- scheduleData()
      df$Nominal <- scales::dollar(df$Nominal)
      df$Real <- scales::dollar(df$Real)
      df
    })
    
    # Nominal Projection Plot
    output$nominalPlot <- renderPlotly({
      df <- scheduleData()
      plot_ly(df, x = ~Year, y = ~Nominal, type = 'scatter', mode = 'lines',
              line = list(color = 'blue', width = 2),
              name = "Nominal Value (USD)") %>%
        layout(title = list(text = "Nominal Investment Projection"),
               xaxis = list(title = "Year"),
               yaxis = list(title = "Portfolio Value (USD)", tickformat = ",.0f"),
               margin = list(l = 50, r = 50, b = 50, t = 50))
    })
    
    # Inflation-Adjusted Projection Plot
    output$realPlot <- renderPlotly({
      df <- scheduleData()
      plot_ly(df, x = ~Year, y = ~Real, type = 'scatter', mode = 'lines',
              line = list(color = 'green', width = 2, dash = "dash"),
              name = "Real Value (USD)") %>%
        layout(title = list(text = "Inflation-Adjusted Investment Projection"),
               xaxis = list(title = "Year"),
               yaxis = list(title = "Portfolio Value (USD)", tickformat = ",.0f"),
               margin = list(l = 50, r = 50, b = 50, t = 50))
    })
    
    # Excel Download Handler
    output$download_excel <- downloadHandler(
      filename = function() {
        paste("financial_planning_schedule_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        df <- scheduleData()
        writexl::write_xlsx(df, path = file)
      }
    )
    
  })
}
