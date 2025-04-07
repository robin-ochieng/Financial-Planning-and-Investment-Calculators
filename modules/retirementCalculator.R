# modules/retirementCalculatorModule.R

retirementCalcUI <- function(id) {
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
    # Translate button row
    fluidRow(
      column(width = 12, align = "right",
             actionButton(ns("translate"), "Translate to French", class = "btn-secondary control-button-translate"),
             actionButton(ns("toggleLanguages"), "More Language Options", class = "btn-secondary control-button-translate")
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          h2("Retirement Calculator", class = "page-title"),
          p("Are you on track to save enough for retirement? Use our calculator to check your progress, see how much retirement income you'll have and estimate how much more you should save.",
            style = "margin-top: 10px;")
        )
      )
    ),
    # NEW: Currency Selector
    fluidRow(
      hr(),
      column(
        width = 4,
        bs4Dash::tooltip(
          shiny::tagAppendAttributes(
            selectInput(
              ns("currency"), 
              label = "Select Preferred Currency", 
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

    fluidRow(
      box(
        title = "Personal Details",
        status = "secondary",
        bs4Dash::tooltip(
          shiny::tagAppendAttributes(
            textInput(ns("current_age"), label = "Your current age", value = "35"),
            `data-trigger` = "click"),
          title = "Enter your current age in years",
          placement = "right"
        ),
        bs4Dash::tooltip(
          textInput(ns("retirement_age"), label = "Planned retirement age", value = "65"),
          title = "Enter the age at which you plan to retire",
          placement = "right"
        ),
        bs4Dash::tooltip(
          textInput(ns("life_expectancy"), label = "Life expectancy", value = "85"),
          title = "Enter your expected age at the end of life",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(
            inputId = ns("pre_tax_income"), 
            label = "", 
            value = 80000, 
            decimalPlaces = 0, 
            digitGroupSeparator = ","
            ),
          title = "Enter your annual pre-tax income in USD",
          placement = "right"
        ),
        width = 4, height = "400px"
      ),
      box(
        title = "Financial Details",
        status = "secondary",
        bs4Dash::tooltip(
          textInput(ns("income_growth"), label = "Annual income increase (%)", value = "3"),
          title = "Enter the expected annual percentage increase in your income",
          placement = "right"
        ),
        bs4Dash::tooltip(
          textInput(ns("income_needed"), label = "Income needed after retirement (%)", value = "75"),
          title = "Percentage of your pre-retirement income needed during retirement",
          placement = "right"
        ),
        bs4Dash::tooltip(
          textInput(ns("investment_return"), label = "Average investment return (%)", value = "6"),
          title = "Expected annual return rate on your investments",
          placement = "right"
        ),
        bs4Dash::tooltip(
          textInput(ns("inflation_rate"), label = "Inflation rate (%)", value = "2"),
          title = "Expected annual inflation rate",
          placement = "right"
        ),
        width = 4, height = "400px"
      ),
      box(
        title = "Savings Details",
        status = "secondary",
        bs4Dash::tooltip(
          textInput(ns("future_savings"), label = "Future savings (% of income)", value = "10"),
          title = "Percentage of your income that you plan to save each year",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(
            inputId = ns("current_savings"), 
            label = "", 
            value = 100000, 
            decimalPlaces = 0, 
            digitGroupSeparator = ","
            ),
          title = "Amount you have saved for retirement so far in USD",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(
            inputId = ns("other_income"), 
            label = "", 
            value = 0, 
            decimalPlaces = 0, 
            digitGroupSeparator = ","
            ),
          title = "Any additional income you expect to receive monthly during retirement",
          placement = "right"
        ),
        width = 4, height = "400px"
      )
    ),
    fluidRow(
      box(
        title = "Retirement Expenses",
        status = "secondary",
        bs4Dash::tooltip(
          autonumericInput(
            inputId = ns("monthly_expense"), 
            label = "", 
            value = 3000, 
            decimalPlaces = 0, 
            digitGroupSeparator = ","
            ),
          title = "Estimated monthly expenses during retirement for housing, travel, etc.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(
            inputId = ns("healthcare_cost"), 
            label = "", 
            value = 5000, 
            decimalPlaces = 0, 
            digitGroupSeparator = ","
          ),
          title = "Estimated annual healthcare costs during retirement",
          placement = "right"
        ),
        width = 6, height = "300px"
      ),
      box(
        title = "Income & Withdrawal Strategy",
        status = "secondary",
        bs4Dash::tooltip(
          autonumericInput(
            inputId = ns("social_security"), 
            label = "", 
            value = 1500, 
            decimalPlaces = 0, 
            digitGroupSeparator = ","    
            ),
          title = "Monthly Social Security benefit expected starting at retirement",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(
            inputId = ns("rental_income"), 
            label = "", 
            value = 500, 
            decimalPlaces = 0, 
            digitGroupSeparator = ","      
            ),
          title = "Monthly rental income expected during retirement",
          placement = "right"
        ),
        bs4Dash::tooltip(
          textInput(ns("withdrawal_rate"), label = "Withdrawal Rate (%)", value = "4"),
          title = "Planned annual withdrawal rate from your retirement savings",
          placement = "right"
        ),
        width = 6, height = "300px"
      )
    ),      
      fluidRow(
        column(width = 12, align = "center",
               actionButton(ns("calculate"), "Calculate", class = "btn-primary control-button", style = "margin-bottom: 20px;")
        )
      ),
      fluidRow(
        box(
          title = "Retirement Summary & Recommendations",
          status = "info",
          width = 12,
          htmlOutput(ns("resultText"))
        )
      ),
      fluidRow(
        box(
          title = "Retirement Savings Projection",
          status = "secondary",   
          width = 12, 
          id = ns("savingsBox"),
          plotlyOutput(ns("savingsPlot"))
        )
      )
  )
}


retirementCalcServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive that returns the user-selected currency
    selectedCurrency <- reactive({
      input$currency  # e.g., "USD", "EUR", etc.
    })  

    # ------------------------------------------------------------------
    # 1) Dynamically update input labels (the tooltips remain unchanged)
    # ------------------------------------------------------------------
    observe({
      cur <- selectedCurrency() 
      # Update the labels of the inputs based on the selected currency      
      updateAutonumericInput(
        session, 
        "pre_tax_income",
        label = paste("Current pre-tax income (", cur, "):", sep = "")
      )
      
      updateAutonumericInput(
        session, 
        "current_savings",
        label = paste("Current retirement savings (", cur, "):", sep = "")
      )
      
      updateAutonumericInput(
        session, 
        "other_income",
        label = paste("Other income after retirement (", cur, "/month):", sep = "")
      )
      
      updateAutonumericInput(
        session, 
        "monthly_expense",
        label = paste("Monthly retirement expenses (", cur, "):", sep = "")
      )
      
      updateAutonumericInput(
        session, 
        "healthcare_cost",
        label = paste("Healthcare costs (", cur, "/year):", sep = "")
      )
      
      updateAutonumericInput(
        session, 
        "social_security",
        label = paste("Social Security Benefit (", cur, "/month):", sep = "")
      )
      
      updateAutonumericInput(
        session, 
        "rental_income",
        label = paste("Rental Income (", cur, "/month):", sep = "")
      )
      
    })

    
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
  

    calcResults <- eventReactive(input$calculate, {
      withProgress(message = 'Calculating retirement savings...', value = 0, {
      # Step 1: Convert input values to numeric
      incProgress(0.1, detail = "Processing input values...")
      # Convert input values to numeric
      current_age <- as.numeric(input$current_age)
      retirement_age <- as.numeric(input$retirement_age)
      life_expectancy <- as.numeric(input$life_expectancy)
      current_savings <- as.numeric(input$current_savings)
      future_savings_rate <- as.numeric(input$future_savings) / 100  # as decimal
      income <- as.numeric(input$pre_tax_income)
      income_growth <- as.numeric(input$income_growth) / 100
      investment_return <- as.numeric(input$investment_return) / 100
      inflation_rate <- as.numeric(input$inflation_rate) / 100
      
      # Additional retirement details
      incProgress(0.1, detail = "Gathering retirement details...")
      monthly_expense <- as.numeric(input$monthly_expense)
      healthcare_cost <- as.numeric(input$healthcare_cost)
      social_security <- as.numeric(input$social_security)
      rental_income <- as.numeric(input$rental_income)
      withdrawal_rate <- as.numeric(input$withdrawal_rate) / 100
      
      # Calculate years until retirement
      # Step 3: Calculate years until retirement and savings accumulation
      incProgress(0.2, detail = "Calculating savings accumulation...")      
      years_to_retirement <- retirement_age - current_age
      
      # Calculate future value of retirement savings at retirement
      savings <- numeric(years_to_retirement + 1)
      savings[1] <- current_savings
      annual_income <- income
      for (i in 2:(years_to_retirement + 1)) {
        annual_income <- annual_income * (1 + income_growth)
        savings[i] <- savings[i - 1] * (1 + investment_return) + (annual_income * future_savings_rate)
      }
      total_savings <- tail(savings, 1)
      
      # Adjust retirement expenses for inflation until retirement
      # Step 4: Adjust expenses for inflation and calculate required annual withdrawal
      incProgress(0.3, detail = "Adjusting expenses for inflation...")
      adjusted_monthly_expense <- monthly_expense * ((1 + inflation_rate) ^ years_to_retirement)
      adjusted_healthcare_cost <- healthcare_cost * ((1 + inflation_rate) ^ years_to_retirement)
      
      # Total annual retirement expenses (combining monthly expenses and healthcare)
      total_annual_expense <- (adjusted_monthly_expense * 12) + adjusted_healthcare_cost
      
      # Annual income from Social Security and rental income
      annual_non_savings_income <- (social_security + rental_income) * 12
      
      # Net annual amount needed from savings
      required_annual_withdrawal <- max(total_annual_expense - annual_non_savings_income, 0)
      
      # Sustainable annual withdrawal from savings based on planned withdrawal rate
      # Step 5: Determine sustainable withdrawal and savings duration
      incProgress(0.2, detail = "Finalizing projections...")
      sustainable_withdrawal <- total_savings * withdrawal_rate
      # Estimate savings duration if only using savings (simple division)
      savings_duration <- if(required_annual_withdrawal > 0) {
        round(total_savings / required_annual_withdrawal, 1) 
      } else {
        Inf
      }  # If no withdrawal needed, set duration to infinite
      

      # Step 6: Generate recommendation with conditional coloring
      recommendation <- if (sustainable_withdrawal >= required_annual_withdrawal) {
        "<span style='color: #27ae60; font-weight: bold;'>Your projected savings are sufficient to cover your retirement expenses.</span>"
      } else {
        "<span style='color: #e74c3c; font-weight: bold;'>Your projected savings may be insufficient. Consider increasing your savings rate or exploring additional income sources for retirement.</span>"
      }
      
      incProgress(0.1, detail = "Wrapping up...")
      list(
        total_savings = total_savings,
        required_annual_withdrawal = required_annual_withdrawal,
        sustainable_withdrawal = sustainable_withdrawal,
        savings_duration = savings_duration,
        recommendation = recommendation,
        savings_data = data.frame(Age = current_age:retirement_age, Savings = savings)
      )
    })
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
    
    # 4) Smooth-scroll only after user actually clicks the button
    observeEvent(input$calculate, ignoreInit = TRUE, {
      # Your existing calculation logic here
      shinyjs::runjs(sprintf(
          "document.getElementById('%s').scrollIntoView({behavior: 'smooth'});",
          ns("resultText")
         ))
    })

    # 5) Plot
    output$savingsPlot <- renderPlotly({
      req(calcResults())
      df <- calcResults()$savings_data
      cur <- selectedCurrency()
      # Create a plotly line chart for retirement savings projection
      plot_ly(df, x = ~Age, y = ~Savings, type = "scatter", mode = "lines",
              line = list(color = "#2c3e50", width = 3)) %>% 
        layout(title = list(text = "Retirement Savings Projection", font = list(size = 15, color = "#2c3e50")),
               margin = list(t = 50),
               xaxis = list(title = "Age", showgrid = FALSE, zeroline = FALSE),
               yaxis = list(
                title = paste0("Savings (", cur, ")"),
                showgrid = TRUE,
                zeroline = FALSE
                ),
               hovermode = "x unified",
               plot_bgcolor = "white",
               paper_bgcolor = "white")
    })

        # Define your helper function somewhere in your server code or in a global file:
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
        cur  # fallback: just use the code if unrecognized
      )
    }

    output$resultText <- renderUI({
      req(calcResults())
      res <- calcResults()
      cur <- selectedCurrency()
      symbol <- currencySymbol(cur)

      HTML(paste0(
        "<div style='font-family: \"Nunito\", sans-serif; color: #333; background-color: #f7f7f7; 
            padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); 
            border: 1px solid #ddd; margin-bottom: 20px;'>",
          "<h3 style='margin-top: 0; color: #2c3e50; margin-bottom: 15px;'>Retirement Savings Summary</h3>",
          
          "<ul style='list-style: none; padding-left: 0; font-size: 16px; line-height: 1.5; margin-bottom: 20px;'>",
            "<li style='margin-bottom: 10px; position: relative; padding-left: 24px;'>",
              "<span style='position: absolute; left: 0; color: #2c3e50;'>&#8226;</span>",
              "<strong>Total Savings at Retirement:</strong> ", 
              symbol, " ", format(round(res$total_savings, 0), big.mark = ","), 
            "</li>",
            "<li style='margin-bottom: 10px; position: relative; padding-left: 24px;'>",
              "<span style='position: absolute; left: 0; color: #2c3e50;'>&#8226;</span>",
              "<strong>Annual Withdrawal Needed from Savings:</strong> ", 
              symbol, " ", format(round(res$required_annual_withdrawal, 0), big.mark = ","), 
            "</li>",
            "<li style='margin-bottom: 10px; position: relative; padding-left: 24px;'>",
              "<span style='position: absolute; left: 0; color: #2c3e50;'>&#8226;</span>",
              "<strong>Sustainable Annual Withdrawal (", input$withdrawal_rate, "% of savings):</strong> ", 
              symbol, " ", format(round(res$sustainable_withdrawal, 0), big.mark = ","), 
            "</li>",
            "<li style='margin-bottom: 10px; position: relative; padding-left: 24px;'>",
              "<span style='position: absolute; left: 0; color: #2c3e50;'>&#8226;</span>",
              "<strong>Estimated Savings Duration:</strong> ", 
              if (is.infinite(res$savings_duration)) "N/A" else paste0(res$savings_duration, " years"), 
            "</li>",
          "</ul>",
          
          "<h4 style='margin-top: 0; color: #2c3e50; margin-bottom: 10px;'>Recommendation</h4>",
          "<p style='font-size: 16px; line-height: 1.5; margin: 0;'>", res$recommendation, "</p>",
        "</div>"
      ))
    })
    
  })
}
