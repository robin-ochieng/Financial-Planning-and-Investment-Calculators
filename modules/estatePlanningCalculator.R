# estatePlanningCalculator Module
# Module UI Function for the Estate Planning module
estatePlanningCalcUI <- function(id) {
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
          h2("Estate Planning Calculator", class = "page-title"),
          p("Estate planning is the process of organizing your assets and liabilities to ensure that your wealth is transferred according to your wishes after your passing, while minimizing taxes and legal complications. Enter your assets, liabilities, deductions, and beneficiary distribution to get started.",
            style = "margin-top: 10px;")
        )
      )
    ),
   # Row 1: Assets & Liabilities
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
   # Row 1: Assets & Liabilities
    fluidRow(
      bs4Card(
        title = "Assets",
        status = "primary",
        width = 6,
        collapsible = TRUE,
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("real_estate"),  
                          label = "",  
                          value = 10000000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the current market value of your real estate holdings.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("investments"),  
                          label = "",  
                          value = 5000000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the value of your investment assets such as stocks, bonds, and mutual funds.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("bank_savings"),  
                          label = "",  
                          value = 3000000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the amount in your bank savings accounts.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("business"),  
                          label = "",  
                          value = 8000000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the estimated value of your business interests.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("personal_property"),  
                          label = "",  
                          value = 2000000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the value of personal assets such as vehicles, jewelry, etc.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("other_assets"),  
                          label = "",  
                          value = 1000000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the value of any other assets not listed above.",
          placement = "right"
        )
      ),
      bs4Card(
        title = "Liabilities",
        status = "danger",
        width = 6,
        height = "555px",
        collapsible = TRUE,
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("mortgages"),  
                          label = "",  
                          value = 4000000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the outstanding balance of your mortgages.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("loans"),  
                          label = "",  
                          value = 2000000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the total amount of your personal loans.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("credit_cards"),  
                          label = "",  
                          value = 200000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the total outstanding balance on your credit cards.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("other_liabilities"),  
                          label = "",  
                          value = 500000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the total amount of any other liabilities.",
          placement = "right"
        )
      )
    ),
    # Row 2: Deductions & Beneficiary Distribution
    fluidRow(
      bs4Card(
        title = "Deductions & Estate Duty",
        status = "warning",
        width = 6,
        collapsible = TRUE,
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("funeral_expenses"),  
                          label = "",  
                          value = 500000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the estimated cost of funeral expenses.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("estate_duty_rate"),  
                          label = "Estate Duty Rate (%):", 
                          value = 25,  
                          decimalPlaces = 1,  
                          digitGroupSeparator = ",",  
                          minimumValue = "0"),
          title = "Enter the estate duty rate as a percentage.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("estate_duty_exemption"),  
                          label = "",  
                          value = 5000000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the exemption amount for estate duty.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("other_deductions"),  
                          label = "",  
                          value = 300000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter any other deductions applicable to your estate.",
          placement = "right"
        )
      ),
      bs4Card(
        title = "Beneficiary Distribution (%)",
        status = "info",
        height = "385px",
        width = 6,
        collapsible = TRUE,
        bs4Dash::tooltip(
          numericInput(ns("spouse_pct"), "Spouse (%):", value = 50, min = 0, max = 100, step = 1),
          title = "Enter the percentage of your estate allocated to your spouse.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("children_pct"), "Children (%):", value = 30, min = 0, max = 100, step = 1),
          title = "Enter the percentage of your estate allocated to your children.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("others_pct"), "Other Heirs (%):", value = 20, min = 0, max = 100, step = 1),
          title = "Enter the percentage of your estate allocated to other heirs.",
          placement = "right"
        )
      )
    ), 
   # Row 3: Calculate button
    fluidRow(
      column(
        width = 12,
        align = "center",
        actionButton(ns("calculate"), "Calculate Estate Plan", class = "btn-success control-button", style = "margin-bottom: 15px;") 
      )
    ),
    # Row 4: Estate Summary Output
    fluidRow(
      bs4Card(
        title = "Estate Summary",
        status = "success",
        width = 12,
        collapsible = TRUE,
        id = ns("EstateSummary"),
        uiOutput(ns("summaryUI"))
      )
    ),
    fluidRow(
      bs4Card(
        title = "Distribution Chart",
        status = "info",
        width = 12,
        collapsible = TRUE,
        plotlyOutput(ns("distributionPlot"), height = "400px")
      )
    )
  )
}



# Module server function
estatePlanningCalcServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # -------------------------------------------------------------
    # A) HELPER: Map currency code to symbol
    # -------------------------------------------------------------
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
        cur  # fallback
      )
    }
    
    # B) Format currency with the symbol
    formatCurrency <- function(amount, cur) {
      sym <- currencySymbol(cur)
      paste0(sym, " ", format(round(amount, 0), big.mark = ",", scientific = FALSE))
    }
    
    # -------------------------------------------------------------
    # 1) Dynamically update input labels to reflect selected currency
    # -------------------------------------------------------------
    observe({
      cur <- input$currency  # e.g. "USD", "EUR", etc.
      sym <- currencySymbol(cur)
      
      # Assets
      updateAutonumericInput(
        session, "real_estate", 
        label = paste("Real Estate (", cur, "):", sep = "")
      )
      updateAutonumericInput(
        session, "investments", 
        label = paste("Investments (", cur, "):", sep = "")
      )
      updateAutonumericInput(
        session, "bank_savings",
        label = paste("Bank Savings (", cur, "):", sep = "")
      )
      updateAutonumericInput(
        session, "business",
        label = paste("Business Interests (", cur, "):", sep = "")
      )
      updateAutonumericInput(
        session, "personal_property",
        label = paste("Personal Property (", cur, "):", sep = "")
      )
      updateAutonumericInput(
        session, "other_assets",
        label = paste("Other Assets (", cur, "):", sep = "")
      )
      
      # Liabilities
      updateAutonumericInput(
        session, "mortgages",
        label = paste("Mortgages (", cur, "):", sep = "")
      )
      updateAutonumericInput(
        session, "loans",
        label = paste("Loans (", cur, "):", sep = "")
      )
      updateAutonumericInput(
        session, "credit_cards",
        label = paste("Credit Card Debts (", cur, "):", sep = "")
      )
      updateAutonumericInput(
        session, "other_liabilities",
        label = paste("Other Liabilities (", cur, "):", sep = "")
      )
      
      # Deductions
      updateAutonumericInput(
        session, "funeral_expenses",
        label = paste("Funeral Expenses (", cur, "):", sep = "")
      )
      # estate_duty_rate label stays as % 
      updateAutonumericInput(
        session, "estate_duty_exemption",
        label = paste("Estate Duty Exemption (", cur, "):", sep = "")
      )
      updateAutonumericInput(
        session, "other_deductions",
        label = paste("Other Deductions (", cur, "):", sep = "")
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
    
    observeEvent(input$calculate, {
      # Your existing calculation logic here
       
      # Scroll to projection box
      shinyjs::runjs(
        sprintf(
          "document.getElementById('%s').scrollIntoView({behavior: 'smooth'});",
          ns("EstateSummary")
         )
      )
    })

  # -------------------------------------------------------------
  # EventReactive: calculations occur when the "Calculate Estate Plan" button is pressed.
  estatePlan <- eventReactive(input$calculate, {
    # Total Assets
    total_assets <- input$real_estate + input$investments + input$bank_savings +
      input$business + input$personal_property + input$other_assets
    
    # Total Liabilities
    total_liabilities <- input$mortgages + input$loans + input$credit_cards + input$other_liabilities
    
    # Preliminary Estate Value
    preliminary_estate <- total_assets - total_liabilities
    
    # Calculate Estate Duty: Only applies to the amount above the exemption threshold.
    taxable_amount <- max(preliminary_estate - input$estate_duty_exemption, 0)
    estate_duty <- taxable_amount * (input$estate_duty_rate / 100)
    
    # Additional deductions (funeral expenses plus other deductions)
    additional_deductions <- input$funeral_expenses + input$other_deductions
    
    # Final Estate after all deductions
    final_estate <- preliminary_estate - estate_duty - additional_deductions
    
    # Beneficiary Distribution
    total_pct <- input$spouse_pct + input$children_pct + input$others_pct
    # If percentages don't sum to 100, adjust them proportionally.
    if(total_pct != 100){
      spouse_adj <- input$spouse_pct / total_pct * 100
      children_adj <- input$children_pct / total_pct * 100
      others_adj <- input$others_pct / total_pct * 100
    } else {
      spouse_adj <- input$spouse_pct
      children_adj <- input$children_pct
      others_adj <- input$others_pct
    }
    
    spouse_distribution <- final_estate * (spouse_adj / 100)
    children_distribution <- final_estate * (children_adj / 100)
    others_distribution <- final_estate * (others_adj / 100)
    
    # Build a summary data frame
      summary_items <- list(
        "Total Assets"             = total_assets,
        "Total Liabilities"        = total_liabilities,
        "Preliminary Estate"       = preliminary_estate,
        "Estate Duty"              = estate_duty,
        "Additional Deductions"    = additional_deductions,
        "Final Estate"             = final_estate,
        "Spouse Distribution"      = spouse_distribution,
        "Children Distribution"    = children_distribution,
        "Other Heirs Distribution" = others_distribution
      )

      list(
        summary_items = summary_items,
        total_pct     = total_pct
      )
    }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
    # [2] Smooth scroll on Calculate
    observeEvent(input$calculate, {
      shinyjs::runjs(
        sprintf(
          "document.getElementById('%s').scrollIntoView({behavior: 'smooth'});",
          ns("EstateSummary")
        )
      )
    })

    # [4] A single UI output that styles the summary data & warning
    output$summaryUI <- renderUI({
      plan <- estatePlan()
      sitems <- plan$summary_items
      total_pct <- plan$total_pct
      cur <- input$currency 

      # Build HTML list items
      # E.g. "Total Assets: 25,000,000"
      list_html <- ""
      for (nm in names(sitems)) {
        amt  <- formatCurrency(sitems[[nm]], cur)
        list_html <- paste0(list_html,
          "<li style='margin-bottom: 10px; position: relative; padding-left: 24px;'>",
            "<span style='position: absolute; left: 0; color: #2c3e50;'>&#8226;</span>",
            "<strong>", nm, ":</strong> ", amt,
          "</li>"
        )
      }

      # Check if we need to display a warning
      warning_html <- ""
      if (total_pct != 100) {
        warning_text <- paste0(
          "Warning: The beneficiary distribution percentages sum to ", total_pct,
          "%. They have been scaled proportionally to total 100%."
        )
        warning_html <- paste0(
          "<div style='color: #d9534f; font-weight: bold; margin-top: 10px;'>",
            "<i class='fa fa-exclamation-triangle' style='margin-right: 5px;'></i>",
            warning_text,
          "</div>"
        )
      }
      # -----------------------------
      # 1) Key Action Items (NEW!)
      # -----------------------------
      # This is a simple static list of suggestions.
      # You could make them dynamic, e.g.:
      #   if (sitems[["Final Estate"]] < 0) show some items, else hide them, etc.
      action_items <- paste0(
        "<h4 style='margin-top: 20px; color: #2c3e50;'>Key Action Items</h4>",
        "<ul style='list-style-type: disc; margin-left: 20px; font-size: 15px;'>",
          "<li>Consider drafting or updating a will/trust if you haven't already.</li>",
          "<li>Review or update beneficiary designations on insurance policies and retirement accounts.</li>",
          "<li>Consult a financial advisor or attorney for specialized estate planning strategies.</li>",
          "<li>Ensure your life insurance coverage matches your estate's needs.</li>",
          "<li>Revisit your plan every few years or after major life changes (marriage, child, etc.).</li>",
        "</ul>"
      )

      # Example of conditional logic: If final estate is negative, show an additional bullet
      # Let's say sitems[["Final Estate"]] is negative => user has more debts than assets
      if (sitems[["Final Estate"]] < 0) {
        action_items <- paste0(
          action_items,
          "<div style='margin-top: 10px; color: #d9534f;'>",
            "<strong>Note:</strong> Your final estate is negative. You may wish to focus on reducing liabilities.",
          "</div>"
        )
      }

      # -----------------------------
      # Disclaimer Section (NEW)
      # -----------------------------
      disclaimer_html <- paste0(
        "<div style='font-size:14px; margin-top:20px; color:#555; font-style:italic;'>",
          "<strong>Disclaimer:</strong> This calculator is provided for educational and illustrative purposes only. ",
          "It does not constitute legal or financial advice. Please consult a qualified legal or financial professional ",
          "for personalized guidance regarding your estate planning needs.",
        "</div>"
      )

      # Construct final HTML
      HTML(paste0(
        "<div style='font-family: \"Nunito\", sans-serif; font-size: 16px; color: #333; ",
        "background-color: #f8f9fa; padding: 20px; border-radius: 8px; ",
        "border: 1px solid #ddd; box-shadow: 0 2px 6px rgba(0,0,0,0.1);'>",

          "<h3 style='margin-top: 0; margin-bottom: 20px; color: #2c3e50;'>Estate Summary</h3>",

          "<ul style='list-style-type: none; padding-left: 0; margin-bottom: 0;'>",
            list_html,
          "</ul>",

          warning_html,  # place the warning below the list if needed

          action_items,  # Add the new Key Action Items block

          disclaimer_html,  # Add the disclaimer at the end
          
        "</div>"
      ))
    })

    # Inside estatePlanningCalcServer, after computing spouse_distribution, children_distribution, others_distribution:

      output$distributionPlot <- renderPlotly({
        req(estatePlan())  # Ensure calculations exist
        plan <- estatePlan()
        cur <- input$currency
        
        # Construct a small data frame with labels and amounts
        distribution_df <- data.frame(
          Beneficiary = c("Spouse", "Children", "Others"),
          Amount = c(
            plan$summary_items[["Spouse Distribution"]],
            plan$summary_items[["Children Distribution"]],
            plan$summary_items[["Other Heirs Distribution"]]
          )
        )
        
        # Convert amounts to actual numeric
        distribution_df$Amount <- as.numeric(distribution_df$Amount)
        
        plot_ly(distribution_df, labels = ~Beneficiary, values = ~Amount, type = "pie") %>%
          layout(
            title = list(text = "Final Estate Distribution by Beneficiary"),
            legend = list(orientation = 'h')  # e.g., place legend horizontally at bottom
          )
      })


 })
}
