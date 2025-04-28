# modules/retirementCalculatorModule.R
retirementCalcUI <- function(id) {
  ns <- NS(id)

  logo_bar <- fluidRow(
    class = "logo-bar",                     # you’ll style this in CSS
    column(
      width = 12,
      tags$div(
        class = "logo-wrapper d-flex justify-content-between align-items-center",
        # left-hand logo
        tags$img(
          src   = "images/kenbright.png",
          class = "logo logo-kenbright"
        ),
        # right-hand logo
        tags$img(
          src   = "images/afdb logo.png",
          class = "logo logo-afdb"
        )
      )
    )
  )
  tagList(
    logo_bar,
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
             #actionButton(ns("toggleLanguages"), "More Language Options", class = "btn-secondary control-button-translate")
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

    fluidRow(
      box( 
        title = "Personal Details",
        status = "secondary",
        textInput(
          inputId = ns("current_age"),
          label = label_with_info(
            label_text = "Your current age",
            info_id = ns("current_age_info"),
            popover_title = "Current Age",
            popover_content = "Enter your current age in years. This helps in determining the years left to contribute, compounding periods, and the length of retirement payouts."
          ),
          value = "35"
        ),
        textInput(
          inputId = ns("retirement_age"),
          label = label_with_info(
            label_text = "Planned retirement age",
            info_id = ns("retirement_age_info"),
            popover_title = "Retirement Age",
            popover_content = "Enter the age at which you want to stop full‑time work. This is used to determine the number of saving years that remain and the first year withdrawals start."
          ),
          value = "65"
        ),
        textInput(
          inputId = ns("life_expectancy"),
          label = label_with_info(
            label_text = "Life expectancy",
            info_id = ns("life_expectancy_info"),
            popover_title = "Life expectancy",
            popover_content = "How long you expect (or plan) to live—i.e., the last age the calculator should fund. It sets the retirement horizon so the tool can test whether your savings last the entire period."
          ),
          value = "85"
        ),
        uiOutput(ns("pre_tax_income_ui")),
        width = 4, height = "415px"
      ),
      box(
        title = "Financial Details",
        status = "secondary",
        textInput(
          inputId = ns("income_growth"),
          label = label_with_info(
            label_text = "Annual income increase (%)",
            info_id = ns("income_growth_info"),
            popover_title = "Annual income increase (%)",
            popover_content = "The average percentage raise you expect each year until retirement (before inflation). It lets the calculator grow future contributions realistically."
          ),
          value = "3"
        ),
        textInput(
          inputId = ns("income_needed"),
          label = label_with_info(
            label_text = "Income needed post retirement (%)",
            info_id = ns("income_needed_info"),
            popover_title = "Income needed after retirement (%)",
            popover_content = "The replacement‑rate target—what fraction of your final pre‑retirement income you’ll actually spend each year in retirement. Typical guidance is 60‑80 %."
          ),
          value = "75"
        ),
        textInput(
          inputId = ns("investment_return"),
          label = label_with_info(
            label_text = "Average investment return (% p.a.)",
            info_id = ns("investment_return_info"),
            popover_title = "Average investment return (%) per year",
            popover_content = "The long‑run annual return you expect on your retirement portfolio (after fees, before inflation). It powers the growth of both existing savings and future contributions."
          ),
          value = "6"
        ),
        width = 4, height = "415px"
      ),
      box(
        title = "Savings Details",
        status = "secondary",
        textInput(
          inputId = ns("future_savings"),
          label = label_with_info(
            label_text = "Future savings (% of income)",
            info_id = ns("future_savings_info"),
            popover_title = "Future savings (% of income)",
            popover_content = "The portion of your gross income you intend to set aside every year until retirement (e.g., contributions to pension, 401(k), IRA, etc.). It’s the main driver of your retirement savings growth."
          ),
          value = "10"
        ),
        uiOutput(ns("current_savings_ui")),
        width = 4, height = "415px"
      )
    ),
    fluidRow(
      box(
        title = "Retirement Expenses",
        status = "secondary",
        uiOutput(ns("monthly_expense_ui")),
        uiOutput(ns("healthcare_cost_ui")),
        width = 6, height = "220px"
      ),
      box(
        title = "Income & Withdrawal Strategy",
        status = "secondary",
        uiOutput(ns("other_retirement_income_ui")),
        textInput(
          inputId = ns("withdrawal_rate"),
          label = label_with_info(
            label_text = "Withdrawal Rate (%)",
            info_id = ns("withdrawal_rate_info"),
            popover_title = "Withdrawal Rate (%)",
            popover_content = "The rule‑of‑thumb percentage of your retirement nest egg you plan to withdraw each year (e.g., 4 % “safe‑withdrawal rule”). Determines whether savings are sufficient and how long they will last."
          ),
          value = "4"
        ),
        width = 6, height = "220px"
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

    output$pre_tax_income_ui <- renderUI({
      # grab the current currency
      cur <- input$currency
      
      autonumericInput(
        inputId           = ns("pre_tax_income"),
        label             = label_with_info(
                              paste0("Current pre-tax income (", cur, "):"),
                              ns("pre_tax_income_info"),
                              "Pre-tax Income",
                              "Your gross (pre-tax) income this year in the chosen currency. It is the base for two things:
                               (1) calculating annual contributions (using “Future savings %”) and 
                               (2) benchmarking how much income you may need in retirement (“Income needed %”)."
                            ),
        value             = 3500,
        decimalPlaces     = 0,
        digitGroupSeparator = ","
      )
    })

    output$current_savings_ui <- renderUI({
      # grab the current currency
      cur <- input$currency
      
      autonumericInput(
        inputId           = ns("current_savings"),
        label             = label_with_info(
                              paste("Current savings (DB & DC) (", cur, "):", sep = ""),
                              ns("current_savings_info"),
                              "Current retirement savings (including DB/DC)",
                              "The total balance of all retirement accounts today—pensions, 401(k)/403(b), IRAs, provident funds, etc.—in the selected currency. It is the starting principal for the growth projection."
                            ),
        value             = 10000,
        decimalPlaces     = 0,
        digitGroupSeparator = ","
      )
    })

    output$monthly_expense_ui <- renderUI({
      # grab the current currency
      cur <- input$currency
      
      autonumericInput(
        inputId           = ns("monthly_expense"),
        label             = label_with_info(
                              paste("Monthly retirement expenses (", cur, "):", sep = ""),
                              ns("monthly_expense_info"),
                              "Monthly retirement expenses",
                              "Your expected average monthly living costs once retired (housing, food, utilities, leisure, etc.). Express it in today’s money; the calculator treats it as level spending for simplicity."
                            ),
        value             = 500,
        decimalPlaces     = 0,
        digitGroupSeparator = ","
      )
    })

    output$healthcare_cost_ui <- renderUI({
      # grab the current currency
      cur <- input$currency
      
      autonumericInput(
        inputId           = ns("healthcare_cost"),
        label             = label_with_info(
                              paste("Healthcare costs (", cur, "/year):", sep = ""),
                              ns("healthcare_cost_info"),
                              "Healthcare costs",
                              "Your estimated annual out‑of‑pocket health‑care and insurance premiums during retirement (in current currency terms). Added on top of general living expenses."
                            ),
        value             = 800,
        decimalPlaces     = 0,
        digitGroupSeparator = ","
      )
    })

    output$other_retirement_income_ui <- renderUI({
      # grab the current currency
      cur <- input$currency
      
      autonumericInput(
        inputId           = ns("other_retirement_income"),
        label             = label_with_info(
                              paste("Other retirement income (", cur, "/month):", sep = ""),
                              ns("other_retirement_income_info"),
                              "Other retirement income",
                              "Any predictable monthly income streams in retirement that aren’t drawn from your savings—e.g., Social Security, government pension, annuity payments, rental cash‑flow, or part‑time work."
                            ),
        value             = 300,
        decimalPlaces     = 0,
        digitGroupSeparator = ","
      )
    })

    
    # 2) Translate button
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
    
    # 3) Calculation: Run once at app launch + again when button is clicked
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
  
      # Additional retirement details
      incProgress(0.1, detail = "Gathering retirement details...")
      monthly_expense <- as.numeric(input$monthly_expense)
      healthcare_cost <- as.numeric(input$healthcare_cost)
      withdrawal_rate <- as.numeric(input$withdrawal_rate) / 100
      other_retirement_income <- as.numeric(input$other_retirement_income)

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
      
      # Step 4: Calculate retirement expenses and income needs
      incProgress(0.3, detail = "Finalizing projections...")
      adjusted_monthly_expense <- monthly_expense 
      adjusted_healthcare_cost <- healthcare_cost 
      
      # Total annual retirement expenses (combining monthly expenses and healthcare)
      total_annual_expense <- (adjusted_monthly_expense * 12) + adjusted_healthcare_cost
      
      # Annual income from Social Security and rental income
      annual_non_savings_income <- other_retirement_income  * 12
      
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

      # Step 7: Return a list of relevant results
      list(
        total_savings = total_savings,
        required_annual_withdrawal = required_annual_withdrawal,
        sustainable_withdrawal = sustainable_withdrawal,
        savings_duration = savings_duration,
        recommendation = recommendation,
        savings_data = data.frame(Age = current_age:retirement_age, Savings = savings)
      )
    })
  }, ignoreInit = TRUE, ignoreNULL = FALSE) 

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
        "XOF" = "F CFA",
        "XAF" = "FCFA",
        "NGN" = "₦",
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
