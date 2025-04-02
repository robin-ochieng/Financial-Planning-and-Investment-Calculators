# IRR Calculator Module

# Module UI with proper namespacing
irrCalcUI <- function(id) {
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
          h2(
            "Income Replacement Ratio Calculator",
            class = "page-title"
          ),
          p(
            "An Income Replacement Ratio (IRR) Calculator is a tool used to estimate the percentage of your pre-retirement income that you will need to replace during retirement to maintain your desired standard of living.",
            style = "margin-top: 10px;"
          )
        )
      )
    ),
    fluidRow(
      box(
        status = "success",
        title = "Personal & Retirement Profile", width = 6, height = "700px",
        bs4Dash::tooltip(
          textInput(inputId = ns("name"), label = "Full Name", value = "John Bosco"),
          title = "Enter your full name.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("current_age"), "Current Age", value = 45, min = 18, max = 100),
          title = "Enter your current age in years.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("retirement_age"), "Normal Retirement Age", value = 65, min = 40, max = 70),
          title = "Enter the age at which you plan to retire.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("salary"), label = "Current Monthly Salary (USD):", value = 250000, decimalPlaces = 0, digitGroupSeparator = ","),
          title = "Enter your current monthly salary.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("contribution_rate"), "Total Contribution Rate (%)", value = 25, min = 0, max = 100),
          title = "Enter the percentage of your salary that you contribute to your retirement fund.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("investment_return"), "Investment Return (%)", value = 11, min = 0, max = 100),
          title = "Enter the expected annual return on your investments.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("salary_escalation"), "Salary Escalation (%)", value = 3, min = 0, max = 100),
          title = "Enter the expected annual increase in your salary.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("fund_balance"), label = "Current Fund Balance (USD):", value = 8000000, decimalPlaces = 0, digitGroupSeparator = ","),
          title = "Enter the current balance of your retirement fund.",
          placement = "right"
        )
      ),
      box(
        status = "success",
        title = "Retirement Income & Assumptions", width = 6, height = "700px",
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("social_security"), label = "Social Security (USD/year):", value = 20000, decimalPlaces = 0, digitGroupSeparator = ","),
          title = "Enter your expected annual Social Security benefit in USD.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("pension_income"), label = "Pension (USD/year):", value = 10000, decimalPlaces = 0, digitGroupSeparator = ","),
          title = "Enter your expected annual pension income in USD.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("savings_withdrawal"), label = "Retirement Savings Withdrawal (USD/year):", value = 25000, decimalPlaces = 0, digitGroupSeparator = ","),
          title = "Enter the expected annual withdrawal from your retirement savings in USD.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("desired_IRR"), "Desired Income Replacement Ratio (%)", value = 80, min = 0, max = 100),
          title = "Enter the desired percentage of your pre-retirement income you wish to replace during retirement.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("tax_rate"), "Tax Rate (%)", value = 15, min = 0, max = 100),
          title = "Enter the tax rate applicable to your pension and savings withdrawals.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("inflation_rate"), "Inflation Rate (%)", value = 2, min = 0, max = 100),
          title = "Enter the expected annual inflation rate.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("life_expectancy"), "Life Expectancy (years)", value = 85, min = 0),
          title = "Enter your expected life expectancy.",
          placement = "right"
        )
      )
    ),
    fluidRow(
         column(width = 12, align = "center",
             actionButton(ns("compute"), "Compute", class = "btn-primary control-button", style = "margin-bottom: 20px;")
          )
        ),
    fluidRow(
      box(
        title = "Results", 
        status = "success",
        width = 12, 
        id = ns("Results"),
        height = "700px",
        fluidRow(
          style = "margin-bottom: 10px;", 
          # 1) Title text output
          div(style = "margin-bottom: 10px;", uiOutput(ns("income_ratio_title")))
        ),
        # Replace the 3 value boxes with an unordered list using tags$li
        fluidRow(
          style = "margin-bottom: 5px;",
          column(
            width = 12,
            uiOutput(ns("financial_list"))
          )
        ),
        # 1) Progress bar and message outputs
        fluidRow(
          style = "margin-bottom: 5px;",
          column(
            width = 12,
            uiOutput(ns("progress_bar_ui")),
            uiOutput(ns("progress_bar_message"))
          )
        ),
        # Disclaimer rendered only after clicking the Compute button
        fluidRow(
          style = "margin-bottom: 5px;",
          column(
            width = 12,
            uiOutput(ns("disclaimer"))
          )
        )
      )
    )
  )
}

# IRR Calculator Module Server
irrCalcServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
    
    # Scroll to results box when compute is pressed
    observeEvent(input$compute, {
      shinyjs::runjs(
        sprintf(
          "document.getElementById('%s').scrollIntoView({behavior: 'smooth'});",
          ns("Results")
         )
      )
    })

    observeEvent(input$compute, {
      withProgress(message = 'Computing IRR...', value = 0, {
      # Step 1: Basic Retirement Profile
      incProgress(0.1, detail = "Processing retirement profile...")
      age <- input$current_age
      years_to_retirement <- input$retirement_age - age
      
      incProgress(0.1, detail = "Calculating income details...")
      # Annual pre-retirement income computed from monthly salary
      annual_income <- input$salary * 12
      # Desired income replacement value (in USD/year)
      desired_IRR_value <- annual_income * (input$desired_IRR / 100)
      
      # Step 2: Compute Total Retirement Income
      incProgress(0.15, detail = "Summing income streams...")      
      # Total Retirement Income streams (USD/year)
      total_ret_income <- input$social_security + input$pension_income + input$savings_withdrawal
      
      # Step 3: Tax Adjustments
      incProgress(0.1, detail = "Calculating taxes...")
      # Calculate taxes on the taxable portion (pension and savings withdrawal)
      taxable_income <- input$pension_income + input$savings_withdrawal
      taxes <- taxable_income * (input$tax_rate / 100)
      after_tax_income <- total_ret_income - taxes

      # Step 4: Inflation Adjustment
      incProgress(0.2, detail = "Adjusting for inflation...")      
      # Adjust for inflation over the years until retirement
      inflation_factor <- (1 + input$inflation_rate / 100)^years_to_retirement
      desired_IRR_value_adj <- desired_IRR_value * inflation_factor
      after_tax_income_adj <- after_tax_income * inflation_factor
      
      # Step 5: Determine Shortfall
      incProgress(0.15, detail = "Computing shortfall...")
      shortfall <- desired_IRR_value_adj - after_tax_income_adj

      # Finalize progress
      incProgress(0.1, detail = "Finalizing results...") 

      # ---------------------------
      # 1) IRR Title 
      # ---------------------------
      output$income_ratio_title <- renderUI({
        # Use rounded values for display
        desired_disp <- scales::dollar(round(desired_IRR_value_adj, 0))
        after_tax_disp <- scales::dollar(round(after_tax_income_adj, 0))
        shortfall_disp <- scales::dollar(round(shortfall, 0))
        
        if (shortfall > 0) {
          HTML(paste0(
            "<div style='font-family: \"Nunito\", sans-serif; color: #333;'>",
            "<h4 style='margin-top: 0;'>", input$name, "</h4>",
            "<p>Your projected retirement income of <strong style='color: #d9534f;'>", after_tax_disp, 
            "</strong> per year is <span style='color: #d9534f;'>below</span> your desired replacement of <strong>", 
            desired_disp, "</strong> per year.</p>",
            "<p><em>Shortfall:</em> <strong>", shortfall_disp, "</strong> per year.</p>",
            "</div>"
          ))
        } else {
          HTML(paste0(
            "<div style='font-family: \"Nunito\", sans-serif; color: #333;'>",
            "<h4 style='margin-top: 0;'>", input$name, "</h4>",
            "<p>Your projected retirement income of <strong style='color: #5cb85c;'>", after_tax_disp, 
            "</strong> per year meets or exceeds your desired replacement of <strong>", 
            desired_disp, "</strong> per year.</p>",
            "</div>"
          ))
        }
      })


      
      # ---------------------------
      # 2) Financial List (as an unordered list)
      # ---------------------------
      output$financial_list <- renderUI({
        HTML(paste0(
          "<div style='font-family: \"Nunito\", sans-serif; font-size: 16px; color: #333; line-height: 1.5;'>",
            "<ul style='list-style-type: none; padding-left: 0;'>",
              "<li style='margin-bottom: 10px;'><strong>Total Retirement Income:</strong> ", scales::dollar(total_ret_income), " per year</li>",
              "<li style='margin-bottom: 10px;'><strong>After-Tax Income:</strong> ", scales::dollar(after_tax_income), " per year</li>",
              "<li style='margin-bottom: 10px;'><strong>Desired IRR (Annual Replacement):</strong> ", scales::dollar(desired_IRR_value), " per year</li>",
              "<li style='margin-bottom: 10px;'><strong>Inflation Factor (over ", years_to_retirement, " years):</strong> ", round(inflation_factor, 2), "</li>",
              "<li style='margin-bottom: 20px;'><strong>Shortfall:</strong> ", scales::dollar(shortfall), " per year</li>",
            "</ul>",
          "</div>"
        ))
      })

      
      # ---------------------------
      # 3) Progress Bar and Message
      # ---------------------------
      output$progress_bar_ui <- renderUI({
        # Calculate ratio as percentage of desired replacement met by after-tax income
        ratio <- round((after_tax_income_adj / desired_IRR_value_adj) * 100, 0)
        ratio <- max(min(ratio, 100), 0)
        bar_color <- if (ratio < 60) {
          "bg-danger"
        } else if (ratio < 80) {
          "bg-warning"
        } else {
          "bg-success"
        }
        tags$div(
          class = "progress",
          style = "height: 30px; margin-bottom: 20px;",
          tags$div(
            class = paste("progress-bar", bar_color),
            role = "progressbar",
            style = paste0("width: ", ratio, "%;"),
            `aria-valuenow` = ratio,
            `aria-valuemin` = 0,
            `aria-valuemax` = 100,
            paste0(ratio, "%")
          )
        )
      })
      
      output$progress_bar_message <- renderUI({
        tags$div(
          style = "font-style: italic; color: #555; margin-top: 10px;",
          tags$strong("Guideline:"),
          tags$ul(
            tags$li("Below 60% = Insufficient replacement"),
            tags$li("60-80% = Adequate replacement"),
            tags$li("80-100% = Sufficient replacement")
          )
        )
      })
      
      # ---------------------------
      # 4) Disclaimer
      # ---------------------------
      output$disclaimer <- renderUI({
        tags$div(
          style = "background-color: #f9f9f9; border-radius: 5px; padding: 15px; margin-top: 20px;",
          h4("Disclaimer", style = "font-weight: bold; margin-bottom: 10px;"),
          p(
            style = "font-size: 14px; color: #333;",
            "Note: This calculator is provided as a guide only. The projections are based on your inputs and assumptions. They do not account for changes in market conditions, tax laws, or personal circumstances. Please consult a financial adviser for personalized advice."
          )
        )
      })
    })  # End withProgress
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
    
  })
}