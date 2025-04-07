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
        status = "success",
        title = "Personal & Retirement Profile", width = 6, height = "700px",
        bs4Dash::tooltip(
          textInput(inputId = ns("name"), label = "Full Name", value = "John Bosco"),
          title = "Enter your full name.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("current_age"), "Current Age", value = 50, min = 18, max = 100),
          title = "Enter your current age in years.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("retirement_age"), "Normal Retirement Age", value = 65, min = 40, max = 70),
          title = "Enter the age at which you plan to retire.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("salary"), label = "", value = 65000, decimalPlaces = 0, digitGroupSeparator = ","),
          title = "Enter your current monthly salary.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("contribution_rate"), "Total Contribution Rate (%)", value = 30, min = 0, max = 100),
          title = "Enter the percentage of your salary that you contribute to your retirement fund.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("investment_return"), "Investment Return (%)", value = 11, min = 0, max = 100),
          title = "Enter the expected annual return on your investments.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("salary_escalation"), "Salary Escalation (%)", value = 8, min = 0, max = 100),
          title = "Enter the expected annual increase in your salary.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("fund_balance"), label = "", value = 10000000, decimalPlaces = 0, digitGroupSeparator = ","),
          title = "Enter the current balance of your retirement fund.",
          placement = "right"
        )
      ),
      box(
        status = "success",
        title = "Retirement Income & Assumptions", width = 6, height = "700px",
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("social_security"), label = "", value = 80000, decimalPlaces = 0, digitGroupSeparator = ","),
          title = "Enter your expected annual Social Security benefit.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("pension_income"), label = "", value = 200000, decimalPlaces = 0, digitGroupSeparator = ","),
          title = "Enter your expected annual pension income.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("savings_withdrawal"), label = "", value = 90000, decimalPlaces = 0, digitGroupSeparator = ","),
          title = "Enter the expected annual withdrawal from your retirement savings.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("desired_IRR"), "Desired Income Replacement Ratio (%)", value = 55, min = 0, max = 100),
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
        height = "900px",
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

    # Reactive that returns the user-selected currency
    selectedCurrency <- reactive({
      input$currency  # e.g., "USD", "EUR", etc.
    })

    # (A) HELPER: Map currency code to symbol
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
        cur  # fallback: just show the code
      )
    }

    # (B) HELPER: Format numeric values with the appropriate currency symbol
    #    For example, formatCurrency(12345.67, "USD") => "$ 12,346"
    formatCurrency <- function(amount, cur) {
      sym <- currencySymbol(cur)
      # We'll round to 0 decimals for consistency with your existing code
      paste0(sym, " ", format(round(amount, 0), big.mark = ","))
    }

    # 1) Dynamically update the input labels so they reflect the selected currency
    observe({
      cur <- selectedCurrency()
      # Salary label
      updateAutonumericInput(session, "salary",
        label = paste("Current Monthly Salary (", cur, "):", sep = "")
      )
      # Fund balance label
      updateAutonumericInput(session, "fund_balance",
        label = paste("Current Fund Balance (", cur, "):", sep = "")
      )
      # Social Security label
      updateAutonumericInput(session, "social_security",
        label = paste("Social Security (", cur, "/year):", sep = "")
      )
      # Pension label
      updateAutonumericInput(session, "pension_income",
        label = paste("Pension (", cur, "/year):", sep = "")
      )
      # Savings withdrawal label
      updateAutonumericInput(session, "savings_withdrawal",
        label = paste("Retirement Savings Withdrawal (", cur, "/year):", sep = "")
      )
    })

    # --------------------
    # TRANSLATE BUTTON
    # --------------------
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
        desired_disp    <- formatCurrency(desired_IRR_value_adj, input$currency)
        after_tax_disp  <- formatCurrency(after_tax_income_adj, input$currency)
        shortfall_disp  <- formatCurrency(shortfall, input$currency)
        
        if (shortfall > 0) {
          # If there's a shortfall
          HTML(paste0(
            "<div style='font-family: \"Nunito\", sans-serif; color: #333; ",
            "border-left: 6px solid #d9534f; padding: 15px; background-color: #f8f9fa; ",
            "border-radius: 6px; margin-bottom: 15px;'>",
              "<h4 style='margin-top: 0; margin-bottom: 10px; ",
                "font-weight: bold; color: #d9534f;'>",
                "<i class='fa fa-exclamation-triangle' style='margin-right: 5px;'></i>",
                input$name,
              "</h4>",
              
              "<p style='font-size: 16px; margin-bottom: 10px;'>",
                "Your projected retirement income of <strong style='color: #d9534f;'>", after_tax_disp, 
                "</strong> per year is <span style='color: #d9534f;'>below</span> ",
                "your desired replacement of <strong>", desired_disp, "</strong> per year.",
              "</p>",

              "<p style='font-size: 16px; margin-bottom: 0;'>",
                "<em>Shortfall:</em> <strong>", shortfall_disp, "</strong> per year.",
              "</p>",
            "</div>"
          ))
        } else {
          # If there's no shortfall
          HTML(paste0(
            "<div style='font-family: \"Nunito\", sans-serif; color: #333; ",
            "border-left: 6px solid #5cb85c; padding: 15px; background-color: #f8f9fa; ",
            "border-radius: 6px; margin-bottom: 15px;'>",
              "<h4 style='margin-top: 0; margin-bottom: 10px; ",
                "font-weight: bold; color: #5cb85c;'>",
                "<i class='fa fa-check-circle' style='margin-right: 5px;'></i>",
                input$name,
              "</h4>",
              
              "<p style='font-size: 16px; margin-bottom: 0;'>",
                "Your projected retirement income of <strong style='color: #5cb85c;'>", after_tax_disp, 
                "</strong> per year meets or exceeds your desired replacement of <strong>", 
                desired_disp, "</strong> per year.",
              "</p>",
            "</div>"
          ))
        }
      })



        output$financial_list <- renderUI({
          HTML(paste0(
            "<div style='font-family: \"Nunito\", sans-serif; font-size: 16px; color: #333; line-height: 1.5;'>",
              "<ul style='list-style-type: none; padding-left: 0; margin: 0;'>",
                
                # Total Retirement Income
                "<li style='margin-bottom: 10px; background-color: #f8f9fa; ",
                "padding: 12px; border-left: 4px solid #5cb85c; border-radius: 4px;'>",
                  "<i class='fa fa-money-bill-wave' style='margin-right: 5px; color: #5cb85c;'></i>",
                  "<strong>Total Retirement Income:</strong> ",
                  formatCurrency(total_ret_income, input$currency), 
                  " per year",
                "</li>",
                
                # After-Tax Income
                "<li style='margin-bottom: 10px; background-color: #f8f9fa; ",
                "padding: 12px; border-left: 4px solid #007bff; border-radius: 4px;'>",
                  "<i class='fa fa-money-bill-wave' style='margin-right: 5px; color: #007bff;'></i>",
                  "<strong>After-Tax Income:</strong> ",
                  formatCurrency(after_tax_income, input$currency), 
                  " per year",
                "</li>",
                
                # Desired IRR
                "<li style='margin-bottom: 10px; background-color: #f8f9fa; ",
                "padding: 12px; border-left: 4px solid #f0ad4e; border-radius: 4px;'>",
                  "<i class='fa fa-percentage' style='margin-right: 5px; color: #f0ad4e;'></i>",
                  "<strong>Desired IRR (Annual Replacement):</strong> ",
                  formatCurrency(desired_IRR_value, input$currency),
                  " per year",
                "</li>",
                
                # Inflation Factor
                "<li style='margin-bottom: 10px; background-color: #f8f9fa; ",
                "padding: 12px; border-left: 4px solid #6f42c1; border-radius: 4px;'>",
                  "<i class='fa fa-chart-line' style='margin-right: 5px; color: #6f42c1;'></i>",
                  "<strong>Inflation Factor (over ", years_to_retirement, " years):</strong> ",
                  round(inflation_factor, 2),
                "</li>",
                
                # Shortfall
                "<li style='margin-bottom: 20px; background-color: #f8f9fa; ",
                "padding: 12px; border-left: 4px solid #d9534f; border-radius: 4px;'>",
                  "<i class='fa fa-exclamation-triangle' style='margin-right: 5px; color: #d9534f;'></i>",
                  "<strong>Shortfall:</strong> ",
                  formatCurrency(shortfall, input$currency),
                  " per year",
                "</li>",
                
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
        
        # Determine color-coded thresholds
        if (ratio < 60) {
          bar_color        <- "bg-danger"   # progress bar class
          replacement_text <- "Insufficient"
          text_color       <- "#d9534f"
        } else if (ratio < 80) {
          bar_color        <- "bg-warning"
          replacement_text <- "Adequate"
          text_color       <- "#f0ad4e"
        } else {
          bar_color        <- "bg-success"
          replacement_text <- "Sufficient"
          text_color       <- "#5cb85c"
        }
        
        # Create the UI output with a descriptive message and the progress bar
        tagList(
          tags$div(
            style = paste0(
              "background-color: #f8f9fa; ",
              "border-left: 6px solid ", text_color, "; ",
              "padding: 15px; margin-bottom: 15px; border-radius: 6px;"
            ),
            # Optionally add an icon for each scenario (uncomment if you like):
            # If you want a different icon for each threshold, you can branch as well
            # e.g. if (ratio < 60) icon = "fa-exclamation-triangle" else ...
            # For simplicity, let's show one icon for all:
            # "<i class='fa fa-chart-line' style='margin-right: 5px; color:", text_color, ";'></i>"
            
            tags$div(
              style = paste0("margin-bottom: 10px; font-size: 22px; font-weight: bold; color:", text_color, ";"),
              paste0("Your Income Replacement Ratio is ", replacement_text, ": ", ratio, "%")
            ),
            
            tags$div(
              class = "progress",
              style = "height: 30px; margin-bottom: 0;",
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
          style = paste0(
            "background-color: #f8f9fa; ",
            "border-left: 6px solid #f0ad4e; ",  # 'warning' color, tweak as you like
            "padding: 15px; ",
            "margin-top: 20px; ",
            "border-radius: 6px;"
          ),
          
          # Heading row: icon + "Disclaimer"
          tags$div(
            style = "display: flex; align-items: center; margin-bottom: 10px;",
            tags$i(
              class = "fa fa-exclamation-circle",
              style = "font-size: 24px; margin-right: 8px; color: #f0ad4e;"  # match the left border color
            ),
            tags$h4("Disclaimer", style = "font-weight: bold; margin: 0;")
          ),
          
          # Body text
          p(
            style = "font-size: 14px; color: #333; margin-bottom: 0;",
            "Note: This calculator is provided as a guide only. The projections are based on your inputs and assumptions. ",
            "They do not account for changes in market conditions, tax laws, or personal circumstances. ",
            "Please consult a financial adviser for personalized advice."
          )
        )
      })

    })  # End withProgress
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
    
  })
}