# estatePlanningCalculator Module
# Module UI Function for the Estate Planning module
estatePlanningCalcUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
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
   # Row 1: Assets & Liabilities
    fluidRow(
      bs4Card(
        title = "Assets",
        status = "primary",
        width = 6,
        collapsible = TRUE,
          uiOutput(ns("real_estate_ui")),
          uiOutput(ns("investments_ui")),
          uiOutput(ns("bank_savings_ui")),
          uiOutput(ns("business_ui")),
          uiOutput(ns("personal_property_ui")),
          uiOutput(ns("other_assets_ui"))
      ),
      bs4Card(
        title = "Liabilities",
        status = "danger",
        width = 6,
        height = "555px",
        collapsible = TRUE,
          uiOutput(ns("mortgages_ui")),
          uiOutput(ns("loans_ui")),
          uiOutput(ns("credit_cards_ui")),
          uiOutput(ns("other_liabilities_ui"))
      )
    ),
    # Row 2: Deductions & Beneficiary Distribution
    fluidRow(
      bs4Card(
        title = "Deductions & Estate Duty",
        status = "warning",
        width = 6,
        collapsible = TRUE,
          uiOutput(ns("funeral_expenses_ui")),
          numericInput(
            inputId = ns("estate_duty_rate"), 
            label = label_with_info(
              label_text = "Estate Duty Rate (%):",
              info_id = ns("estate_duty_rate_info"),
              popover_title = "Estate Duty Rate (%)",
              popover_content = "Enter the percentage rate of estate duty that applies to your estate. This is the tax levied on the transfer of your estate after your death."
            ),
            value = 25, 
            min = 0, 
            max = 100, 
            step = 1
          ),
          uiOutput(ns("estate_duty_exemption_ui")),
          uiOutput(ns("other_deductions_ui"))
      ),
      bs4Card(
        title = "Beneficiary Distribution (%)",
        status = "info",
        height = "385px",
        width = 6,
        collapsible = TRUE,
        bs4Dash::tooltip(
          numericInput(
            inputId = ns("spouse_pct"), 
            label = label_with_info(
              label_text = "Spouse (%):",
              info_id = ns("spouse_pct_info"),
              popover_title = "Spouse (%)",
              popover_content = "Enter the percentage of your estate allocated to your spouse. This can include a partner or significant other."
            ),
            value = 50, 
            min = 0, 
            max = 100, 
            step = 1
          ),
          title = "Enter the percentage of your estate allocated to your spouse.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(
            inputId = ns("children_pct"), 
            label = label_with_info(
              label_text = "Children (%):",
              info_id = ns("children_pct_info"),
              popover_title = "Children (%)",
              popover_content = "Enter the percentage of your estate allocated to your children. This can include biological children, stepchildren, or adopted"
            ),
            value = 30, 
            min = 0, 
            max = 100, 
            step = 1
          ),
          title = "Enter the percentage of your estate allocated to your children. This can include biological children, stepchildren, or adopted children.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(
            inputId = ns("others_pct"), 
            label = label_with_info(
              label_text = "Other Heirs (%):",
              info_id = ns("others_pct_info"),
              popover_title = "Other Heirs (%)",
              popover_content = "Enter the percentage of your estate allocated to other heirs. This can include siblings, parents, or other relatives. It's important to specify how you want your estate divided among them."
            ),
            value = 20, 
            min = 0, 
            max = 100, 
            step = 1
          ),
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
    
    # B) Format currency with the symbol
    formatCurrency <- function(amount, cur) {
      sym <- currencySymbol(cur)
      paste0(sym, " ", format(round(amount, 0), big.mark = ",", scientific = FALSE))
    }
    
    # -------------------------------------------------------------
    # 1) Dynamically update input labels to reflect selected currency
    # -------------------------------------------------------------
    output$real_estate_ui <- renderUI({
      cur <- input$currency
      autonumericInput(
        inputId           = ns("real_estate"),
        label             = label_with_info(
                              paste("Real Estate (", cur, "):", sep = ""),
                              ns("real_estate_info"),
                              "Real Estate",
                              "Enter the total value of all real estate properties you own, including your primary residence and any rental properties."
                            ),
        value             = 10000000,
        decimalPlaces     = 0,
        digitGroupSeparator = ","
      )
    })

      output$investments_ui <- renderUI({
        cur <- input$currency
        autonumericInput(
          inputId           = ns("investments"),
          label             = label_with_info(
                                paste("Investments (", cur, "):", sep = ""),
                                ns("investments_info"),
                                "Investments",
                                "Enter the total value of all investments, including stocks, bonds, and mutual funds."
                              ),
          value             = 5000000,
          decimalPlaces     = 0,
          digitGroupSeparator = ","
        )
      })

      output$bank_savings_ui <- renderUI({
        cur <- input$currency
        autonumericInput(
          inputId           = ns("bank_savings"),
          label             = label_with_info(
                                paste("Bank Savings (", cur, "):", sep = ""),
                                ns("bank_savings_info"),
                                "Bank Savings",
                                "Enter the total value of all bank accounts, including checking and savings accounts."
                              ),
          value             = 3000000,
          decimalPlaces     = 0,
          digitGroupSeparator = ","
        )
      })

      output$business_ui <- renderUI({
        cur <- input$currency
        autonumericInput(
          inputId           = ns("business"),
          label             = label_with_info(
                                paste("Business Interests (", cur, "):", sep = ""),
                                ns("business_info"),
                                "Business Interests",
                                "Enter the total value of all business interests, including ownership stakes in companies, partnerships, and sole proprietorships."
                              ),
          value             = 8000000,
          decimalPlaces     = 0,
          digitGroupSeparator = ","
        )
      })

      output$personal_property_ui <- renderUI({
        cur <- input$currency
        autonumericInput(
          inputId           = ns("personal_property"),
          label             = label_with_info(
                                paste("Personal Property (", cur, "):", sep = ""),
                                ns("personal_property_info"),
                                "Personal Property",
                                "Enter the total value of all personal property, including vehicles, jewelry, land and collectibles."
                              ),
          value             = 2000000,
          decimalPlaces     = 0,
          digitGroupSeparator = ","
        )
      })

      output$other_assets_ui <- renderUI({
        cur <- input$currency
        autonumericInput(
          inputId           = ns("other_assets"),
          label             = label_with_info(
                                paste("Other Assets (", cur, "):", sep = ""),
                                ns("other_assets_info"),
                                "Other Assets",
                                "Enter the total value of any other assets not covered above."
                              ),
          value             = 1000000,
          decimalPlaces     = 0,
          digitGroupSeparator = ","
        )
      })

      output$mortgages_ui <- renderUI({
        cur <- input$currency
        autonumericInput(
          inputId           = ns("mortgages"),
          label             = label_with_info(
                                paste("Mortgages (", cur, "):", sep = ""),
                                ns("mortgages_info"),
                                "Mortgages",
                                "Enter the total value of all mortgages on your properties."
                              ),
          value             = 4000000,
          decimalPlaces     = 0,
          digitGroupSeparator = ","
        )
      })

      output$loans_ui <- renderUI({
        cur <- input$currency
        autonumericInput(
          inputId           = ns("loans"),
          label             = label_with_info(
                                paste("Loans (", cur, "):", sep = ""),
                                ns("loans_info"),
                                "Loans",
                                "Enter the total value of all loans, including personal loans, student loans, and auto loans."
                              ),
          value             = 2000000,
          decimalPlaces     = 0,
          digitGroupSeparator = ","
        )
      })

      output$credit_cards_ui <- renderUI({
        cur <- input$currency
        autonumericInput(
          inputId           = ns("credit_cards"),
          label             = label_with_info(
                                paste("Credit Card Debts (", cur, "):", sep = ""),
                                ns("credit_cards_info"),
                                "Credit Card Debts",
                                "Enter the total value of all credit card debts."
                              ),
          value             = 200000,
          decimalPlaces     = 0,
          digitGroupSeparator = ","
        )
      })

      output$other_liabilities_ui <- renderUI({
        cur <- input$currency
        autonumericInput(
          inputId           = ns("other_liabilities"),
          label             = label_with_info(
                                paste("Other Liabilities (", cur, "):", sep = ""),
                                ns("other_liabilities_info"),
                                "Other Liabilities",
                                "Enter the total value of any other liabilities not covered above."
                              ),
          value             = 500000,
          decimalPlaces     = 0,
          digitGroupSeparator = ","
        )
      })

      output$funeral_expenses_ui <- renderUI({
        cur <- input$currency
        autonumericInput(
          inputId           = ns("funeral_expenses"),
          label             = label_with_info(
                                paste("Funeral Expenses (", cur, "):", sep = ""),
                                ns("funeral_expenses_info"),
                                "Funeral Expenses",
                                "Enter the estimated cost of funeral and burial expenses."
                              ),
          value             = 500000,
          decimalPlaces     = 0,
          digitGroupSeparator = ","
        )
      })

      output$estate_duty_exemption_ui <- renderUI({
        cur <- input$currency
        numericInput(
          inputId           = ns("estate_duty_exemption"),
          label             = label_with_info(
                                paste("Estate Duty Exemption (", cur, "):", sep = ""),
                                ns("estate_duty_exemption_info"),
                                "Estate Duty Exemption",
                                "Enter the amount exempt from estate duty. This is the threshold above which estate duty applies."
                              ),
          value             = 25,
          min               = 0,
          max               = 100,
          step              = 1
        )
      })

      output$other_deductions_ui <- renderUI({
        cur <- input$currency
        autonumericInput(
          inputId           = ns("other_deductions"),
          label             = label_with_info(
                                paste("Other Deductions (", cur, "):", sep = ""),
                                ns("other_deductions_info"),
                                "Other Deductions",
                                "Enter the total value of any other deductions not covered above. This can include debts, funeral expenses, and other liabilities."
                              ),
          value             = 300000,
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
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
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

    # -------------------------------------------------------------
 })
}
