# personalInvestmentCalculator Module
# Module UI: Defines the layout for inputs and outputs
personalInvestmentCalcUI <- function(id) {
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
          h2("Personal Investment Calculator", class = "page-title"),
          p("A Personal Investment Calculator provides a clear picture of how your investments may grow over time and helps you make informed decisions to achieve your financial goals.",
            style = "margin-top: 10px;")
        )
      )
    ),
    # -----------------------
    # 1) CURRENCY SELECTOR
    # -----------------------
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
      # Inputs box on the left
      box(
        status = "secondary",
        title = "Investment Inputs", width = 5, height = "580px", 
        # Tooltips for each field
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("initial"), label = "", value = 100000, decimalPlaces = 0, digitGroupSeparator = ","),
          title = "The lump sum you invest at the start.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("contribution"), label = "", value = 50000, decimalPlaces = 0, digitGroupSeparator = ","),
          title = "The amount you add to your investment every month.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("rate"), "Annual Interest Rate (%):", value = 7, min = 0, step = 0.1),
          title = "The expected annual return on your investment.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("years"), "Investment Duration (years):", value = 20, min = 1, step = 1),
          title = "How many years you plan to invest.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("inflation"), "Inflation Rate (%):", value = 2, min = 0, step = 0.1),
          title = "The expected annual inflation rate (optional).",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("goal"), label = "", value = 500000, decimalPlaces = 0, digitGroupSeparator = ","),
          title = "A target amount you want to achieve, which can help determine how much you need to save or invest.",
          placement = "right"
        )
      ),
      # Results box on the right
      box(
        title = "Results Summary",
        status = "secondary",
        width = 7,
        height = "580px",
        id = ns("ResultsSummary"),
        fluidRow(
          div(style = "margin-bottom: 10px;", uiOutput(ns("investment_summary")))
        ),
        fluidRow(
          valueBoxOutput(ns("final_balance_box"), width = 12)
        ),
        fluidRow(
          downloadButton(ns("download_excel"), "Download Investment Schedule (Excel)", class = "btn-success control-button1", style = "margin-top: 10px;")
        )
      )
    ),
      # Row 3: Calculate button
      fluidRow(
        column(
          width = 12,
          align = "center",
        actionButton(ns("calculate"), "Calculate", class = "btn-success control-button", style = "margin-bottom: 15px;")
        )
      ),
    # Graphs on the bottom: Two sets of graphs (Nominal and Inflation-Adjusted)
    fluidRow(
      box(
        title = "Investment Growth - Nominal", status = "secondary", width = 12,
        plotlyOutput(ns("growthPlot_nominal"))
      )
    ),
    fluidRow(
      box(
        title = "Investment Growth - Inflation Adjusted", status = "secondary", width = 12,
        plotlyOutput(ns("growthPlot_real"))
      )
    ),
    fluidRow(
      box(
        title = "Summary Table", status = "secondary", width = 12,
        dataTableOutput(ns("summaryTable"))
      )
    )
  )
}


# Module Server: Contains the reactive calculations and output renderings
personalInvestmentCalcServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ----------------------------------------------------------------
    # A) HELPER FUNCTION: Map currency code to symbol
    # ----------------------------------------------------------------
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
        cur  # fallback if no match
      )
    }
    
    # ----------------------------------------------------------------
    # B) HELPER FUNCTION: Format amounts using selected currency symbol
    # ----------------------------------------------------------------
    formatCurrency <- function(amount, cur) {
      sym <- currencySymbol(cur)
      paste0(sym, " ", format(round(amount, 0), big.mark = ","))
    }

    # Reactive that returns the user-selected currency
    selectedCurrency <- reactive({
      input$currency  # e.g., "USD", "EUR", etc.
    })
    
    # ----------------------------------------------------------------
    # 1) Dynamically update input labels to reflect selected currency
    # ----------------------------------------------------------------
    observe({
      cur <- selectedCurrency() 
      sym <- currencySymbol(input$currency)
      updateAutonumericInput(session, "initial", 
                             label = paste("Initial Investment (", cur, "):", sep = ""))
      updateAutonumericInput(session, "contribution", 
                             label = paste("Monthly Contribution (", cur, "):", sep = ""))
      updateAutonumericInput(session, "goal", 
                             label = paste("Goal Amount (", cur, "):", sep = ""))
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
   
    # Reactive calculation with progress bar
    calculate_investment <- eventReactive(input$calculate, {
      withProgress(message = 'Calculating investment growth...', value = 0, {
        n <- 3  # Number of major steps

      # Convert inputs to numeric values
      initial      <- as.numeric(input$initial)
      monthly      <- as.numeric(input$contribution)
      rate_annual  <- as.numeric(input$rate) / 100
      monthly_rate <- rate_annual / 12
      years        <- as.numeric(input$years)
      months       <- years * 12
      inflation_rate <- as.numeric(input$inflation) / 100
      goal         <- as.numeric(input$goal)
      incProgress(1/n, detail = 'Processing inputs...')
      
      # --- Closed-Form Calculations for Summary ---
      # Future Value of Initial Investment using compound interest formula:
      fv_initial <- initial * (1 + monthly_rate)^(months)
      
      # Future Value of Regular Contributions (Annuity Formula):
      fv_annuity <- if(monthly_rate == 0) { 
        monthly * months 
      } else { 
        monthly * ((1 + monthly_rate)^(months) - 1) / monthly_rate 
      }
      
      # Total Nominal Future Value:
      total_nominal <- fv_initial + fv_annuity
      
      # Inflation-Adjusted (Real) Future Value:
      total_real <- total_nominal / ((1 + inflation_rate)^(years))
      incProgress(1/n, detail = 'Performing calculations...')

      # --- Monthly Schedule (for plots and table) ---
      schedule <- data.frame(
        Month = 1:months, 
        Nominal = numeric(months), 
        Real = numeric(months)
      )

      # Use an iterative approach to simulate month-by-month growth
      schedule$Nominal[1] <- initial * (1 + monthly_rate) + monthly
      if(months > 1) {
        for (i in 2:months) {
          schedule$Nominal[i] <- (schedule$Nominal[i - 1] + monthly) * (1 + monthly_rate)
          incProgress(1/(n * months), detail = paste('Calculating month', i, 'of', months))
        }
      }
      # Adjust each month’s nominal value for inflation (using monthly approximation)
      schedule$Real <- schedule$Nominal / ((1 + inflation_rate)^((schedule$Month)/12))
      incProgress(1/n, detail = 'Finalizing results...')

      list(
        schedule = schedule,
        fv_initial = fv_initial,
        fv_annuity = fv_annuity,
        total_nominal = total_nominal,
        total_real = total_real,
        goal = goal
      )
      })
    }, ignoreInit = FALSE, ignoreNULL = FALSE)

    # Scroll to projection box when compute is pressed
    observeEvent(input$calculate, {
      shinyjs::runjs(
        sprintf(
          "document.getElementById('%s').scrollIntoView({behavior: 'smooth'});",
          ns("ResultsSummary")
        )
      )
    })    
    
    # Nominal Growth Plot
    output$growthPlot_nominal <- renderPlotly({
      req(calculate_investment())  # Ensure the calculation is done before plotting
      df <- calculate_investment()$schedule
      cur <- selectedCurrency()
      plot_ly(df, x = ~Month, y = ~Nominal, type = 'scatter', mode = 'lines',
              line = list(color = 'blue', width = 2)) %>%
        layout(title = list(text = "Nominal Investment Growth Over Time"),
               xaxis = list(title = "Months"),
               yaxis = list(title = paste0("Balance (", cur, ")")),
               margin = list(l = 50, r = 50, b = 50, t = 50))
    })
    
    # Inflation-Adjusted Growth Plot
    output$growthPlot_real <- renderPlotly({
      req(calculate_investment())  # Ensure the calculation is done before plotting
      df <- calculate_investment()$schedule
      cur <- selectedCurrency()
      plot_ly(df, x = ~Month, y = ~Real, type = 'scatter', mode = 'lines',
              line = list(color = 'green', width = 2)) %>%
        layout(title = list(text = "Inflation-Adjusted Investment Growth Over Time"),
               xaxis = list(title = "Months"),
               yaxis = list(title = paste0("Balance (", cur, ")")),
               margin = list(l = 50, r = 50, b = 50, t = 50))
    })
    
    # Summary Table of monthly values with additional columns
    output$summaryTable <- renderDataTable({
      req(calculate_investment())  # Ensure the calculation is done before rendering the table
      calc <- calculate_investment()
      df <- calc$schedule
      
      # Add additional columns to match the download version
      df$Cumulative_Contributions <- cumsum(rep(input$contribution, nrow(df)))
      df$Total_Contributions <- input$initial + df$Cumulative_Contributions
      df$Total_Interest_Earned <- df$Nominal - df$Total_Contributions
      df$Month_Year <- format(seq(Sys.Date(), by = "month", length.out = nrow(df)), "%Y-%m")
      
      # Format columns with our custom currency function
      df$Nominal <- formatCurrency(df$Nominal, input$currency)
      df$Real <- formatCurrency(df$Real, input$currency)
      df$Cumulative_Contributions <- formatCurrency(df$Cumulative_Contributions, input$currency)
      df$Total_Contributions <- formatCurrency(df$Total_Contributions, input$currency)
      df$Total_Interest_Earned <- formatCurrency(df$Total_Interest_Earned, input$currency)
      
      datatable(
      df,
      options = list(
        scrollX = TRUE,      # Enable horizontal scrolling
        scrollY = '400px',   # Enable vertical scrolling with a fixed height
        paging = FALSE       # Disable pagination so the scroll appears over the full dataset
       )
     )
    })

    output$investment_summary <- renderUI({
      calc <- calculate_investment()
      cur  <- input$currency
      symbol <- currencySymbol(cur)

      # Begin the main container
      summary_html <- paste0(
        "<div style='font-family: \"Nunito\", sans-serif; 
                    background-color: #f8f9fa; 
                    padding: 20px; 
                    border: 1px solid #ddd; 
                    border-radius: 8px; 
                    margin-bottom: 20px;'>",
          "<h3 style='margin-top: 0; color: #2c3e50;'>Investment Summary</h3>",

          # Start the custom-bullet UL
          "<ul style='list-style: none; padding-left: 0; font-size: 16px; line-height: 1.6; margin-bottom: 20px;'>",

            # 1) Future Value of Initial Investment
            "<li style='margin-bottom: 10px; position: relative; padding-left: 24px;'>",
              "<span style='position: absolute; left: 0; color: #2c3e50;'>&#8226;</span>",
              "<strong>Future Value of Initial Investment:</strong> ",
              formatCurrency(round(calc$fv_initial, 0), cur),
            "</li>",

            # 2) Future Value of Regular Contributions
            "<li style='margin-bottom: 10px; position: relative; padding-left: 24px;'>",
              "<span style='position: absolute; left: 0; color: #2c3e50;'>&#8226;</span>",
              "<strong>Future Value of Regular Contributions:</strong> ",
              formatCurrency(round(calc$fv_annuity, 0), cur),
            "</li>",

            # 3) Total Future Value (Nominal)
            "<li style='margin-bottom: 10px; position: relative; padding-left: 24px;'>",
              "<span style='position: absolute; left: 0; color: #2c3e50;'>&#8226;</span>",
              "<strong>Total Future Value (Nominal):</strong> ",
              formatCurrency(round(calc$total_nominal, 0), cur),
            "</li>",

            # 4) Total Future Value (Inflation-Adjusted)
            "<li style='margin-bottom: 10px; position: relative; padding-left: 24px;'>",
              "<span style='position: absolute; left: 0; color: #2c3e50;'>&#8226;</span>",
              "<strong>Total Future Value (Inflation-Adjusted):</strong> ",
              formatCurrency(round(calc$total_real, 0), cur),
            "</li>",

            # 5) Goal Amount
            "<li style='margin-bottom: 10px; position: relative; padding-left: 24px;'>",
              "<span style='position: absolute; left: 0; color: #2c3e50;'>&#8226;</span>",
              "<strong>Goal Amount:</strong> ",
              paste0(symbol, " ", formatC(calc$goal, format = "f", big.mark = ",", digits = 0)),
            "</li>",

          "</ul>"
      )

      # Recommendation message based on whether the goal is met
      recommendation <- if (calc$total_nominal >= calc$goal) {
        paste0(
          "<h4 style='margin-top: 0; color: #2c3e50; margin-bottom: 10px;'>Recommendation</h4>",
          "<p style='font-size: 16px; margin-bottom: 0; color: green; font-weight: bold;'>",
            "Congratulations! You are on track to meet your investment goal.",
          "</p>"
        )
      } else {
        paste0(
          "<h4 style='margin-top: 0; color: #2c3e50; margin-bottom: 10px;'>Recommendation</h4>",
          "<p style='font-size: 16px; margin-bottom: 0; color: red; font-weight: bold;'>",
            "<strong>Result:</strong> Your projected future value is below your goal. ",
            "Consider increasing your contributions or adjusting your strategy.",
          "</p>"
        )
      }

      # Close out the container
      summary_html <- paste0(summary_html, recommendation, "</div>")
      
      HTML(summary_html)
    })


    # --- In your server (or module server) ---
    output$final_balance_box <- renderUI({
      calc <- calculate_investment()
      cur  <- input$currency
      
      # Choose color (green if goal met, red if not)
      box_color <- if (calc$total_nominal >= calc$goal) "#27ae60" else "#c0392b"
      
      # Create a custom card-like HTML block
      HTML(paste0(
        "<div style='padding: 20px; border: 2px solid ", box_color, 
            "; border-radius: 5px; background-color: #f9f9f9;'>",
          "<h4 style='margin-top: 0; margin-bottom: 10px; color: ", box_color, ";'>Final Nominal Balance</h4>",
          "<p style='font-size: 24px; font-weight: bold; margin-bottom: 0; color: ", box_color, ";'>",
            formatCurrency(round(calc$total_nominal, 0), cur),
          "</p>",
        "</div>"
      ))
    })

    
    # Download Handler for Excel export of the schedule
    # Updated Download Handler for Formatted Excel Output
    output$download_excel <- downloadHandler(
      filename = function() {
        paste("investment_schedule_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        library(openxlsx)
        calc <- calculate_investment()
        
        # Compute additional fields
        schedule <- calc$schedule
        schedule$Cumulative_Contributions <- cumsum(rep(input$contribution, nrow(schedule)))
        schedule$Total_Contributions <- input$initial + schedule$Cumulative_Contributions
        schedule$Total_Interest_Earned <- schedule$Nominal - schedule$Total_Contributions
        
        # Convert month index to Year-Month format
        schedule$Month_Year <- format(seq(Sys.Date(), by = "month", length.out = nrow(schedule)), "%Y-%m")
        
        # Create a workbook
        wb <- createWorkbook()
        addWorksheet(wb, "Investment Schedule")
        
        # Define styles
        header_style <- createStyle(fontSize = 12, fontColour = "white", fgFill = "#0137A6", halign = "CENTER", textDecoration = "bold")
        currency_style <- createStyle(numFmt = "\"$\"#,##0.00")
        
        # Write data with formatting
        writeData(wb, "Investment Schedule", schedule, startRow = 1, headerStyle = header_style)
        addStyle(wb, "Investment Schedule", currency_style, cols = 2:5, rows = 2:(nrow(schedule) + 1), gridExpand = TRUE)
        
        # Save workbook
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    
  })
}
