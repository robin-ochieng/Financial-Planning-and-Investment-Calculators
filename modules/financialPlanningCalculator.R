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
    # shinyjs::useShinyjs(),
    # # Include Google Translate scripts in the header
    # tags$head(
    #   tags$script(src = "https://translate.google.com/translate_a/element.js?cb=googleTranslateElementInit"),
    #   tags$script(HTML("
    #     function googleTranslateElementInit() {
    #       new google.translate.TranslateElement({
    #         pageLanguage: 'en', 
    #         autoDisplay: false
    #       }, 'google_translate_element');
    #     }
    #   "))
    # ),
    # # Position the Google Translate widget off-screen so it's still accessible
    # tags$div(id = "google_translate_element", style = "position: absolute; left: -9999px;"),
    # fluidRow(
    #   column(width = 12, align = "right",
    #          actionButton(ns("translate"), "Translate to French", class = "btn-secondary control-button-translate"),
    #          actionButton(ns("toggleLanguages"), "More Language Options", class = "btn-secondary control-button-translate")
    #   )
    # ),
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
      column(
        width = 4,
        bs4Dash::tooltip(
          selectInput(
            inputId = ns("goal"),
            label = label_with_info(
              label_text = "Select your Financial Goal:",
              info_id = ns("goal_info"),
              popover_title = "Select your Financial Goal",
              popover_content = "Select your financial goal. For each goal, default minimum amounts and terms are defined."
            ),
            choices = names(goalSettings),
            selected = "Building a House"
          ),
          title = "Select your financial goal. For each goal, default minimum amounts and terms are defined.",
          placement = "right"
        )
      ),
      hr(),
      column(width = 4,
        bs4Dash::tooltip(
          shiny::tagAppendAttributes(
            selectInput(
              inputId = ns("currency"),
              label = label_with_info(
                label_text = "Select Preferred Currency",
                info_id = ns("currency_info"),
                popover_title = "Select Preferred Currency",
                popover_content = "Select the currency in which results should be displayed."
              ), 
              choices = list(
                "US Dollar (USD)" = "USD",
                "Euro (EUR)" = "EUR",
                "British Pound (GBP)" = "GBP",
                "Japanese Yen (JPY)" = "JPY",
                "Swiss Franc (CHF)" = "CHF",
                "Canadian Dollar (CAD)" = "CAD",
                "Australian Dollar (AUD)" = "AUD",
                "Kenyan Shilling (KES)" = "KES",
                "West African CFA franc (XOF)" = "XOF",
                "Central African CFA franc (XAF)" = "XAF",
                "Nigerian Naira (NGN)" = "NGN"                    
                ), 
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
        # Annual Income
          uiOutput(ns("income_ui")),
        # Monthly Expenses
          uiOutput(ns("expenses_ui")),
        # Current Savings
          uiOutput(ns("savings_ui")),
        # Total Debt
          uiOutput(ns("debt_ui")),
        # Emergency Fund
          uiOutput(ns("emergency_ui"))
        ),
        bs4Card(
        title = "Goal Settings & Economic Assumptions",
        status = "secondary",
        width = 6,
        height = "800px",
        collapsible = TRUE,
        # Goal Amount (auto defaults based on selected goal if desired)
          uiOutput(ns("goal_amount_ui")),
        # Goal Term in years
        bs4Dash::tooltip(
          numericInput(
            inputId = ns("goal_term"), 
            label = label_with_info(
              label_text = "Goal Term (years):",
              info_id = ns("goal_term_info"),
              popover_title = "Goal Term (years)",
              popover_content = "Enter the number of years by which you want to achieve your goal. This is the time frame in which you plan to reach your financial target. It is used to calculate the future value of your investments."
            ),
            value = 5, 
            min = 0.25, 
            step = 0.25
          ),
          title = "Enter the number of years by which you want to achieve your goal.",
          placement = "right"
        ),
         # Expected Rate of Return
        bs4Dash::tooltip(
          numericInput(
            inputId = ns("exp_return"),
            label = label_with_info(
              label_text = "Expected Annual Return (%):",
              info_id = ns("exp_return_info"),
              popover_title = "Expected Annual Return (%)",
              popover_content = "Enter the expected annual rate of return on your investments. This is the rate at which you expect your investments to grow annually. It is used to calculate the future value of your investments."
            ),
            value = 7, 
            min = 0, 
            step = 0.1
          ),
          title = "Enter the expected annual rate of return on your investments.",
          placement = "right"
        )                       
      )
    ),
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
        "XOF" = "F CFA",
        "XAF" = "FCFA",
        "NGN" = "₦",
        cur  # fallback: just use the code if unrecognized
      )
    }
    
    # 2) Format amount with the chosen currency symbol
    formatCurrency <- function(amount, cur) {
      sym <- currencySymbol(cur)
      paste0(sym, " ", format(round(amount, 0), big.mark = ","))
    }
    
    # -----------------------------------------------------------------
    # (B) HELPER: Create a label with an info icon that shows a tooltip on hover
    # -----------------------------------------------------------------
     output$income_ui <- renderUI({
      cur <- input$currency
      autonumericInput(
        inputId           = ns("income"),
        label             = label_with_info(
                              paste0("Annual Income (", cur, "):"),
                              ns("income_info"),
                              "Annual Income",
                              "Enter your total annual income before taxes and deductions. This is the amount you earn in a year. It is used to calculate your savings rate and future value of investments."
                            ),
        value             = 80000,
        decimalPlaces     = 0,
        digitGroupSeparator = ","
      )
    })

    output$expenses_ui <- renderUI({
      cur <- input$currency
      autonumericInput(
        inputId           = ns("expenses"),
        label             = label_with_info(
                              paste0("Monthly Expenses (", cur, "):"),
                              ns("expenses_info"),
                              "Monthly Expenses",
                              "Enter your total monthly expenses. This is the amount you spend in a month. It is used to calculate your annual savings and emergency fund status."
                            ),
        value             = 3000,
        decimalPlaces     = 0,
        digitGroupSeparator = ","
      )
    })


    output$savings_ui <- renderUI({
      cur <- input$currency
      autonumericInput(
        inputId           = ns("savings"),
        label             = label_with_info(
                              paste0("Current Savings (", cur, "):"),
                              ns("savings_info"),
                              "Current Savings",
                              "Enter your current savings. This is the amount you have saved so far. It is used to calculate your net worth and future value of investments."
                            ),
        value             = 200000,
        decimalPlaces     = 0,
        digitGroupSeparator = ","
      )
    })


    output$debt_ui <- renderUI({
      cur <- input$currency
      autonumericInput(
        inputId           = ns("debt"),
        label             = label_with_info(
                              paste0("Total Debt (", cur, "):"),
                              ns("debt_info"),
                              "Total Debt",
                              "Enter your total debt. This is the amount you owe to creditors. It is used to calculate your net worth."              
                            ),
        value             = 30000,
        decimalPlaces     = 0,
        digitGroupSeparator = ","
      )
    })

    output$emergency_ui <- renderUI({
      cur <- input$currency
      autonumericInput(
        inputId           = ns("emergency"),
        label             = label_with_info(
                              paste0("Emergency Fund (", cur, "):"),
                              ns("emergency_info"),
                              "Emergency Fund",
                              "Enter the amount you have set aside for emergencies. This is the amount you have saved for unexpected expenses. It is used to assess your emergency fund status."
                            ),
        value             = 15000,
        decimalPlaces     = 0,
        digitGroupSeparator = ","
      )
    })

    output$goal_amount_ui <- renderUI({
      cur <- input$currency
      autonumericInput(
        inputId           = ns("goal_amount"),
        label             = label_with_info(
                              paste0("Goal Amount (", cur, "):"),
                              ns("goal_amount_info"),
                              "Goal Amount",
                              "Enter the total amount you want to save for your goal. This is the target amount you want to achieve by the end of the goal term. It is used to calculate the required monthly savings."
                            ),
        value             = 50000,
        decimalPlaces     = 0,
        digitGroupSeparator = ","
      )
    })



    # # When Translate button is clicked, trigger translation using the dropdown
    # observeEvent(input$translate, {
    #   shinyjs::runjs("
    #     function triggerTranslation() {
    #       var combo = document.querySelector('.goog-te-combo');
    #       if (combo) {
    #         combo.value = 'fr';
    #         // Create and dispatch a change event to trigger translation
    #         var event = document.createEvent('HTMLEvents');
    #         event.initEvent('change', true, true);
    #         combo.dispatchEvent(event);
    #       } else {
    #         console.log('Google Translate combo element not found.');
    #       }
    #     }
    #     // Allow extra time for the widget to load
    #     setTimeout(triggerTranslation, 1500);
    #   ")
    # })

    # # Button to toggle visibility of the language options (with scrolling).
    # observeEvent(input$toggleLanguages, {
    #   shinyjs::runjs("
    #     var el = document.getElementById('google_translate_element');
    #     // If currently hidden off-screen, make it visible and scrollable.
    #     if (el.style.left === '-9999px') {
    #       el.style.left = '0';
    #       el.style.position = 'relative';
    #       el.style.maxHeight = '300px';
    #       el.style.overflowY = 'auto';
    #     } else {
    #       el.style.left = '-9999px';
    #     }
    #   ")
    # })
    
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
        net_worth <- input$savings - input$debt
        
        # Annual Savings: Income minus annual expenses
        annual_savings <- input$income - (input$expenses * 12)
        
        # Savings Rate (%)
        savings_rate <- (annual_savings / input$income) * 100
        
        # Step 2: Compute Future Value of Investments  
        incProgress(0.2, detail = "Computing future value of investments...")  
        # Compound interest rate
        r <- input$exp_return / 100
        n <- input$goal_term  # number of years until goal
        total_principal <- input$savings 
        # Future Value of non-retirement investments
        fv_nominal <- total_principal * (1 + r)^n + annual_savings * (((1 + r)^n - 1) / r)

        # For all goals, use fv_nominal as the future projection
        total_future <- fv_nominal
        gap <- max(input$goal_amount - total_future, 0)
        req_monthly <- if(gap > 0) gap / (n * 12) else 0

        # Step 4: Assess Insurance and Emergency Fund Status
        incProgress(0.2, detail = "Assessing emergency fund status...")     
        # Emergency Fund sufficiency (recommend at least 3 months of expenses)
        emergency_status <- if(input$emergency >= (input$expenses * 3)) "Sufficient" else "Insufficient"
        
      # Finalize results
      incProgress(0.2, detail = "Finalizing results...")
      list(net_worth = net_worth,
           annual_savings = annual_savings,
           savings_rate = savings_rate,
           fv_nominal = fv_nominal,
           total_future = total_future,
           gap = gap,
           req_monthly = req_monthly,
           emergency_status = emergency_status)
      })
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    
    # -----------------------------------------------------------------
    # F) RESULT SUMMARY
    # -----------------------------------------------------------------
    output$fp_summary <- renderUI({
      data <- fpData()
      cur  <- input$currency
      
      summary_html <- paste0(
        "<div style='font-family: \"Nunito\", sans-serif; background-color: #f9f9f9; padding: 25px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>",
          "<h3 style='margin-top: 0; color: #2c3e50;'>Financial Summary</h3>",
          "<div style='margin-bottom:10px; font-size:18px; color: #2c3e50;'><strong>Net Worth:</strong> ", formatCurrency(data$net_worth, cur), "</div>",
          "<div style='margin-bottom:10px; font-size:18px; color: #2c3e50;'><strong>Savings Rate:</strong> ", sprintf("%.1f", data$savings_rate), "%</div>",
          "<div style='margin-bottom:10px; font-size:18px; color: #2c3e50;'><strong>Future Value of Savings:</strong> ", formatCurrency(data$total_future, cur), "</div>",
          "<div style='margin-bottom:10px; font-size:18px; color: #2c3e50;'><strong>Required Monthly Savings for Goal:</strong> ", formatCurrency(data$req_monthly, cur), "</div>",
          "<div style='margin-bottom:10px; font-size:18px; color: #2c3e50;'><strong>Emergency Fund:</strong> ", data$emergency_status, "</div>"
      )
      
      recommendation <- if (data$gap > 0) {
        paste0(
          "<div style='font-size:18px; margin-top:15px; color: #d9534f;'>",
            "<strong>Recommendation:</strong> You need to save an additional ",
            formatCurrency(data$req_monthly, cur), " per month to reach your goal within ",
            input$goal_term, " years.",
          "</div>"
        )
      } else {
        "<div style='font-size:18px; margin-top:15px; color: #5cb85c;'><strong>Recommendation:</strong> Congratulations! Your current savings strategy meets your goal.</div>"
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
      total_principal <- input$savings
      nominal <- total_principal * (1 + r)^years + annual_savings * (((1 + r)^years - 1) / r)
      
      data.frame(Year = years, Nominal = nominal)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    
    output$scheduleTable <- renderDataTable({
      df <- scheduleData()
      total_principal <- input$savings
      annual_savings <- input$income - (input$expenses * 12)
      df$Cumulative_Contributions <- total_principal + annual_savings * df$Year
      df$Total_Interest <- df$Nominal - df$Cumulative_Contributions
      df$Month_Year <- paste0("Year ", df$Year)
      
      cur <- input$currency
      df$Nominal <- sapply(df$Nominal, function(x) formatCurrency(x, cur))
      df$Cumulative_Contributions <- sapply(df$Cumulative_Contributions, function(x) formatCurrency(x, cur))
      df$Total_Interest <- sapply(df$Total_Interest, function(x) formatCurrency(x, cur))
      
      df
    }, options = list(
      scrollX = TRUE,
      scrollY = '400px',
      paging = FALSE
    ))
    
    
    
    # -----------------------------------------------------------------
    # H) EXCEL DOWNLOAD HANDLER
    # -----------------------------------------------------------------
    output$download_excel <- downloadHandler(
      filename = function() {
        paste("financial_planning_schedule_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        df <- scheduleData()
        total_principal <- input$savings
        annual_savings <- input$income - (input$expenses * 12)
        df$Cumulative_Contributions <- total_principal + annual_savings * df$Year
        df$Total_Interest <- df$Nominal - df$Cumulative_Contributions
        
        wb <- createWorkbook()
        addWorksheet(wb, "Schedule")
        
        headerStyle <- createStyle(
          fontSize = 12, 
          fontColour = "white", 
          fgFill = "#0137A6", 
          halign = "CENTER", 
          textDecoration = "bold"
        )
        currencyStyle <- createStyle(numFmt = "\"$\"#,##0.00")
        
        writeData(wb, sheet = "Schedule", df, headerStyle = headerStyle)
        cols_to_format <- c("Nominal", "Cumulative_Contributions", "Total_Interest")
        colNumbers <- which(names(df) %in% cols_to_format)
        addStyle(wb, sheet = "Schedule", style = currencyStyle, 
                 rows = 2:(nrow(df) + 1), cols = colNumbers, gridExpand = TRUE)
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    
  })
}