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
    shinyjs::useShinyjs(),
    # Include Google Translate scripts in the header
    tags$head(
      tags$script(src = "https://translate.google.com/translate_a/element.js?cb=googleTranslateElementInit"),
      tags$script(HTML("
        function googleTranslateElementInit() {
          new google.translate.TranslateElement({
            pageLanguage: 'en', 
            autoDisplay: false
          }, 'google_translate_element');
        }
      "))
    ),
    # Position the Google Translate widget off-screen so it's still accessible
    tags$div(id = "google_translate_element", style = "position: absolute; left: -9999px;"),
    fluidRow(
      column(width = 12, align = "right",
             actionButton(ns("translate"), "Translate to French", class = "btn-secondary control-button-translate"),
             #actionButton(ns("toggleLanguages"), "More Language Options", class = "btn-secondary control-button-translate")
      )
    ),
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
    fluidRow(
      hr(),
      column(width = 4,
        bs4Dash::tooltip(
          shiny::tagAppendAttributes(
            selectInput(
              ns("currency"),
              label = "Select Desired Currency:",
              choices = c("USD", "EUR", "GBP", "JPY", "CHF", "CAD", "AUD", "KES"),
              selected = "USD"
            ),
            `data-trigger` = "click"
          ),
          title = "Select the currency in which results should be displayed",
          placement = "right"
        )
      )
    ),   
    # Input Parameters in a collapsible card
    fluidRow(
      bs4Card(
        title = "Personal Financial Profile",
        status = "secondary",
        width = 6,
        height = "800px",
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
                           label = "",  
                           value = 80000, 
                           decimalPlaces = 0, 
                           digitGroupSeparator = ","),
          title = "Enter your current annual income in USD.",
          placement = "right"
        ),
        # Monthly Expenses
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("expenses"), 
                           label = "",  
                           value = 3000, 
                           decimalPlaces = 0, 
                           digitGroupSeparator = ","),
          title = "Enter your average monthly expenses in USD.",
          placement = "right"
        ),
        # Current Savings
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("savings"), 
                           label = "",  
                           value = 20000, 
                           decimalPlaces = 0, 
                           digitGroupSeparator = ","),
          title = "Enter the total amount you currently have saved.",
          placement = "right"
        ),
        # Total Debt
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("debt"), 
                           label = "",  
                           value = 30000, 
                           decimalPlaces = 0, 
                           digitGroupSeparator = ","),
          title = "Enter your current total debt.",
          placement = "right"
        ),
         # Investment Portfolio
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("portfolio"), 
                           label = "",  
                           value = 50000, 
                           decimalPlaces = 0, 
                           digitGroupSeparator = ","),
          title = "Enter the total value of your investment portfolio.",
          placement = "right"
        ),
        # Life Insurance Coverage
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("insurance"), 
                           label = "",  
                           value = 500000, 
                           decimalPlaces = 0, 
                           digitGroupSeparator = ","),
          title = "Enter your current life insurance coverage amount in USD.",
          placement = "right"
        ),
        # Retirement Age
        bs4Dash::tooltip(
          numericInput(ns("retirement_age"), "Retirement Age:", value = 65, min = 18, step = 1),
          title = "Enter the age at which you plan to retire.",
          placement = "right"
        ),
        # Current Retirement Savings
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("retirement_savings"), 
                           label = "",  
                           value = 100000, 
                           decimalPlaces = 0, 
                           digitGroupSeparator = ","),
          title = "Enter the amount you currently have in retirement savings.",
          placement = "right"
        )
        ),
        bs4Card(
        title = "Goal Settings & Economic Assumptions",
        status = "secondary",
        width = 6,
        height = "800px",
        collapsible = TRUE,
        # Emergency Fund
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("emergency"), 
                           label = "",  
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
                           label = "",  
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
        title = "Results Summary", 
        status = "secondary", 
        width = 12,
        id = ns("ResultsSummary"),
        fluidRow(
          div(style = "margin-bottom: 10px;", uiOutput(ns("fp_summary")))
        ),
        fluidRow(
          downloadButton(ns("download_excel"), "Download Schedule (Excel)", class = "btn-info control-button1")
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

    # -----------------------------------------------------------------
    # A) HELPER FUNCTIONS FOR MULTICURRENCY
    # -----------------------------------------------------------------
    
    # 1) Map currency code to symbol
    currencySymbol <- function(cur) {
      switch(cur,
        "USD" = "$",
        "EUR" = "€",
        "GBP" = "£",
        "JPY" = "¥",
        "CHF" = "Fr",
        "CAD" = "C$",
        "AUD" = "A$",
        "KES" = "KSh.",
        cur  # fallback if unrecognized
      )
    }
    
    # 2) Format amount with the chosen currency symbol
    formatCurrency <- function(amount, cur) {
      sym <- currencySymbol(cur)
      paste0(sym, " ", format(round(amount, 0), big.mark = ","))
    }
    
    # -----------------------------------------------------------------
    # B) DYNAMICALLY UPDATE INPUT LABELS BASED ON SELECTED CURRENCY
    # -----------------------------------------------------------------
    observe({
      cur <- input$currency  # e.g. "USD", "EUR"
      sym <- currencySymbol(cur)
      
      # Re-label each input to reflect the chosen currency
      updateAutonumericInput(session, "income", 
        label = paste0("Annual Income (", cur, "):")
      )
      updateAutonumericInput(session, "expenses", 
        label = paste0("Monthly Expenses (", cur, "):")
      )
      updateAutonumericInput(session, "savings", 
        label = paste0("Current Savings (", cur, "):")
      )
      updateAutonumericInput(session, "debt", 
        label = paste0("Total Debt (", cur, "):")
      )
      updateAutonumericInput(session, "portfolio", 
        label = paste0("Investment Portfolio (", cur, "):")
      )
      updateAutonumericInput(session, "insurance", 
        label = paste0("Life Insurance Coverage (", cur, "):")
      )
      updateAutonumericInput(session, "retirement_savings",
        label = paste0("Current Retirement Savings (", cur, "):")
      )
      updateAutonumericInput(session, "emergency", 
        label = paste0("Emergency Fund (", cur, "):")
      )
      updateAutonumericInput(session, "goal_amount",
        label = paste0("Goal Amount (", cur, "):")
      )
    })

    # -----------------------------------------------------------------
    # C) EXISTING TRANSLATION + SMOOTH SCROLL
    # -----------------------------------------------------------------
    # When Translate button is clicked, trigger translation using the dropdown
    observeEvent(input$translate, {
      shinyjs::runjs("
        function triggerTranslation() {
          var combo = document.querySelector('.goog-te-combo');
          if (combo) {
            combo.value = 'fr';
            // Create and dispatch a change event to trigger translation
            var event = document.createEvent('HTMLEvents');
            event.initEvent('change', true, true);
            combo.dispatchEvent(event);
          } else {
            console.log('Google Translate combo element not found.');
          }
        }
        // Allow extra time for the widget to load
        setTimeout(triggerTranslation, 1500);
      ")
    })

    # Button to toggle visibility of the language options (with scrolling).
    observeEvent(input$toggleLanguages, {
      shinyjs::runjs("
        var el = document.getElementById('google_translate_element');
        // If currently hidden off-screen, make it visible and scrollable.
        if (el.style.left === '-9999px') {
          el.style.left = '0';
          el.style.position = 'relative';
          el.style.maxHeight = '300px';
          el.style.overflowY = 'auto';
        } else {
          el.style.left = '-9999px';
        }
      ")
    })
    
    observeEvent(input$update, {
      # Scroll to projection box
      shinyjs::runjs(
        sprintf(
          "document.getElementById('%s').scrollIntoView({behavior: 'smooth'});",
          ns("ResultsSummary")
         )
      )
    })    
    # Update default Goal Amount and Goal Term based on selected goal
    observeEvent(input$goal, {
      req(goalSettings[[input$goal]])
      updateNumericInput(session, "goal_term", value = goalSettings[[input$goal]]$defaultTerm)
      updateNumericInput(session, "goal_amount", value = goalSettings[[input$goal]]$defaultAmount)
    })
    
    # Reactive: Financial Profile Calculation triggered by "update" button
    fpData <- eventReactive(input$update, {
      withProgress(message = "Calculating financial profile...", value = 0, {
        # Step 1: Compute Net Worth and Annual Savings
        incProgress(0.2, detail = "Calculating Net Worth and Annual Savings...")
      
        # Net Worth calculation: Assets (Savings + Investments) minus Debt
        net_worth <- (input$savings + input$portfolio) - input$debt
        
        # Annual Savings: Income minus annual expenses
        annual_savings <- input$income - (input$expenses * 12)
        
        # Savings Rate (%)
        savings_rate <- (annual_savings / input$income) * 100
        
        # Step 2: Compute Future Value of Investments  
        incProgress(0.2, detail = "Computing future value of investments...")  
        # Compound interest rate
        r <- input$exp_return / 100
        n <- input$goal_term  # number of years until goal
        total_principal <- input$savings + input$portfolio
        # Future Value of non-retirement investments
        fv_nominal <- total_principal * (1 + r)^n + annual_savings * (((1 + r)^n - 1) / r)

        # Step 3: Adjust for Retirement Savings if Goal is Retirement
        incProgress(0.2, detail = "Adjusting for retirement savings...")      
        # For "Retirement" goal, add current retirement savings grown to future value
        if (input$goal == "Retirement") {
          retirement_future <- input$retirement_savings * (1 + r)^n
          total_future <- fv_nominal + retirement_future
          gap <- max(input$goal_amount - total_future, 0)
        } else {
          retirement_future <- NA
          total_future <- fv_nominal
          gap <- max(input$goal_amount - fv_nominal, 0)
        }
        req_monthly <- if(gap > 0) gap / (n * 12) else 0

        # Step 4: Assess Insurance and Emergency Fund Status
        incProgress(0.2, detail = "Assessing insurance and emergency fund status...")     
        # Insurance Coverage adequacy (threshold: $500,000)
        insurance_status <- if(input$insurance >= 500000) "Adequate" else "Insufficient"
        # Emergency Fund sufficiency (recommend at least 3 months of expenses)
        emergency_status <- if(input$emergency >= (input$expenses * 3)) "Sufficient" else "Insufficient"
        
      # Finalize results
      incProgress(0.2, detail = "Finalizing results...")
      list(net_worth = net_worth,
           annual_savings = annual_savings,
           savings_rate = savings_rate,
           fv_nominal = fv_nominal,
           retirement_future = retirement_future,
           total_future = total_future,
           gap = gap,
           req_monthly = req_monthly,
           insurance_status = insurance_status,
           emergency_status = emergency_status)
      })
    }, ignoreInit = FALSE, ignoreNULL = FALSE)
    
    # -----------------------------------------------------------------
    # F) RESULT SUMMARY
    # -----------------------------------------------------------------
    output$fp_summary <- renderUI({
      data <- fpData()
      cur  <- input$currency
      
      # Build summary details using formatCurrency
      summary_html <- paste0(
        "<div style='font-family: \"Nunito\", sans-serif; background-color: #f9f9f9; padding: 25px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>",
          "<h3 style='margin-top: 0; color: #2c3e50;'>Financial Summary</h3>",
          
          "<div style='margin-bottom:10px; font-size:18px; color: #2c3e50;'>",
            "<strong>Net Worth:</strong> ", formatCurrency(data$net_worth, cur),
          "</div>",
          
          "<div style='margin-bottom:10px; font-size:18px; color: #2c3e50;'>",
            "<strong>Savings Rate:</strong> ", sprintf("%.1f", data$savings_rate), "%</div>"
      )
      
      if (input$goal == "Retirement") {
        summary_html <- paste0(
          summary_html,
          "<div style='margin-bottom:10px; font-size:18px; color: #2c3e50;'><strong>Future Value of Non-Retirement Investments:</strong> ",
            formatCurrency(data$fv_nominal, cur), "</div>",
          "<div style='margin-bottom:10px; font-size:18px; color: #2c3e50;'><strong>Future Value of Retirement Savings:</strong> ",
            formatCurrency(data$retirement_future, cur), "</div>",
          "<div style='margin-bottom:10px; font-size:18px; color: #2c3e50;'><strong>Total Projected Retirement Savings:</strong> ",
            formatCurrency(data$total_future, cur), "</div>",
          "<div style='margin-bottom:10px; font-size:18px; color: #2c3e50;'><strong>Retirement Savings Gap:</strong> ",
            formatCurrency(data$gap, cur), "</div>"
        )
      } else {
        summary_html <- paste0(
          summary_html,
          "<div style='margin-bottom:10px; font-size:18px; color: #2c3e50;'><strong>Future Value of Investments:</strong> ",
            formatCurrency(data$fv_nominal, cur), "</div>",
          "<div style='margin-bottom:10px; font-size:18px; color: #2c3e50;'><strong>Required Monthly Savings for Goal:</strong> ",
            formatCurrency(data$req_monthly, cur), "</div>"
        )
      }
      
      # Insurance & Emergency
      summary_html <- paste0(
        summary_html,
        "<div style='margin-bottom:10px; font-size:18px; color: #2c3e50;'><strong>Life Insurance Coverage:</strong> ", data$insurance_status, "</div>",
        "<div style='margin-bottom:10px; font-size:18px; color: #2c3e50;'><strong>Emergency Fund:</strong> ", data$emergency_status, "</div>"
      )
      
      # Recommendation
      recommendation <- if (data$gap > 0) {
        paste0(
          "<div style='font-size:18px; margin-top:15px; color: #d9534f;'>",
            "<strong>Recommendation:</strong> You need to save an additional ",
            formatCurrency(data$req_monthly, cur), " per month to reach your goal within ",
            input$goal_term, " years.",
          "</div>"
        )
      } else {
        "<div style='font-size:18px; margin-top:15px; color: #5cb85c;'><strong>Recommendation:</strong> Congratulations! Your current savings and investment strategy meet your goal.</div>"
      }
      
      summary_html <- paste0(summary_html, recommendation, "</div>")
      HTML(summary_html)
    })
    
    # -----------------------------------------------------------------
    # G) SCHEDULE DATA
    # -----------------------------------------------------------------
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
      # For retirement goal, also calculate retirement savings projection
      if (input$goal == "Retirement") {
        retirement <- input$retirement_savings * (1 + r)^years
        data.frame(Year = years, Nominal = nominal, Real = real, Retirement = retirement)
      } else {
        data.frame(Year = years, Nominal = nominal, Real = real)
      }
    })
    
    output$scheduleTable <- renderDataTable({
      df <- scheduleData()
      total_principal <- input$savings + input$portfolio
      annual_savings <- input$income - (input$expenses * 12)
      df$Cumulative_Contributions <- total_principal + annual_savings * df$Year
      df$Total_Interest <- df$Nominal - df$Cumulative_Contributions
      df$Month_Year <- paste0("Year ", df$Year)
      
      # Use our formatCurrency instead of scales::dollar, if you want the custom symbol in the table
      cur <- input$currency
      
      df$Nominal <- sapply(df$Nominal, function(x) formatCurrency(x, cur))
      df$Real <- sapply(df$Real, function(x) formatCurrency(x, cur))
      df$Cumulative_Contributions <- sapply(df$Cumulative_Contributions, function(x) formatCurrency(x, cur))
      df$Total_Interest <- sapply(df$Total_Interest, function(x) formatCurrency(x, cur))
      if ("Retirement" %in% colnames(df)) {
        df$Retirement <- sapply(df$Retirement, function(x) formatCurrency(x, cur))
      }
      
      df
    }, options = list(
      scrollX = TRUE,
      scrollY = '400px',
      paging = FALSE
    ))
    
    # -----------------------------------------------------------------
    # H) NOMINAL PLOT
    # -----------------------------------------------------------------
    output$nominalPlot <- renderPlotly({
      df <- scheduleData()
      cur <- currencySymbol(input$currency)
      
      plot_ly(df, x = ~Year, y = ~Nominal, type = 'scatter', mode = 'lines',
              line = list(color = 'blue', width = 2),
              name = paste("Nominal Value (", cur, ")")) %>%
        layout(
          title = list(text = "Nominal Investment Projection"),
          xaxis = list(title = "Year"),
          yaxis = list(title = paste0("Portfolio Value (", cur, ")")),
          margin = list(l = 50, r = 50, b = 50, t = 50)
        )
    })
    
    # -----------------------------------------------------------------
    # I) REAL PLOT
    # -----------------------------------------------------------------
    output$realPlot <- renderPlotly({
      df <- scheduleData()
      cur <- currencySymbol(input$currency)
      
      plot_ly(df, x = ~Year, y = ~Real, type = 'scatter', mode = 'lines',
              line = list(color = 'green', width = 2, dash = "dash"),
              name = paste("Real Value (", cur, ")")) %>%
        layout(
          title = list(text = "Inflation-Adjusted Investment Projection"),
          xaxis = list(title = "Year"),
          yaxis = list(title = paste0("Portfolio Value (", cur, ")")),
          margin = list(l = 50, r = 50, b = 50, t = 50)
        )
    })
    
    # Excel Download Handler
    # Excel Download Handler with formatting and additional fields
    output$download_excel <- downloadHandler(
      filename = function() {
        paste("financial_planning_schedule_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        # Get schedule data (yearly projection)
        df <- scheduleData()
        
        # Calculate additional fields:
        # Total principal: current savings + investment portfolio
        total_principal <- input$savings + input$portfolio
        # Annual savings: income minus (12 * monthly expenses)
        annual_savings <- input$income - (input$expenses * 12)
        
        # Cumulative Contributions: starting with total_principal, then adding annual savings each year
        df$Cumulative_Contributions <- total_principal + annual_savings * df$Year
        # Total Interest Earned: difference between Nominal projection and contributions
        df$Total_Interest <- df$Nominal - df$Cumulative_Contributions
        
        # Create a new workbook and add a worksheet
        wb <- createWorkbook()
        addWorksheet(wb, "Schedule")
        
        # Define styles for headers and currency formatting
        headerStyle <- createStyle(
          fontSize = 12, 
          fontColour = "white", 
          fgFill = "#0137A6", 
          halign = "CENTER", 
          textDecoration = "bold"
        )
        currencyStyle <- createStyle(numFmt = "\"$\"#,##0.00")
        
        # Write the data with header style
        writeData(wb, sheet = "Schedule", df, headerStyle = headerStyle)
        
        # Identify columns to format as currency: Nominal, Real, Cumulative Contributions, Total Interest, and Retirement (if applicable)
        cols_to_format <- c("Nominal", "Real", "Cumulative_Contributions", "Total_Interest")
        if ("Retirement" %in% names(df)) {
          cols_to_format <- c(cols_to_format, "Retirement")
        }
        colNumbers <- which(names(df) %in% cols_to_format)
        
        # Apply the currency style to the selected columns (starting from row 2 because row 1 has headers)
        addStyle(wb, sheet = "Schedule", style = currencyStyle, 
                rows = 2:(nrow(df) + 1), cols = colNumbers, gridExpand = TRUE)
        
        # Save the workbook to the specified file
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    
  })
}