# personalInvestmentCalculator Module
# Module UI: Defines the layout for inputs and outputs
personalInvestmentCalcUI <- function(id) {
  ns <- NS(id)
  tagList(
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
    fluidRow(
      # Inputs box on the left
      box(
        status = "secondary",
        title = "Investment Inputs", width = 5, height = "580px", 
        # Tooltips for each field
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("initial"), label = "Initial Investment (USD):", value = 10000, decimalPlaces = 0, digitGroupSeparator = ","),
          title = "The lump sum you invest at the start.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("contribution"), label = "Monthly Contribution (USD):", value = 500, decimalPlaces = 0, digitGroupSeparator = ","),
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
          numericInput(ns("inflation"), "Inflation Rate (%) [Optional]:", value = 2, min = 0, step = 0.1),
          title = "The expected annual inflation rate (optional).",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("goal"), label = "Goal Amount (USD):", value = 500000, decimalPlaces = 0, digitGroupSeparator = ","),
          title = "A target amount you want to achieve, which can help determine how much you need to save or invest.",
          placement = "right"
        )
      ),
      # Results box on the right
      box(
        title = "Results Summary", status = "secondary", width = 7, height = "580px",        fluidRow(
          div(style = "margin-bottom: 10px;", uiOutput(ns("investment_summary")))
        ),
        fluidRow(
          valueBoxOutput(ns("final_balance_box"), width = 12)
        ),
        fluidRow(
          downloadButton(ns("download_excel"), "Download Investment Schedule (Excel)", class = "btn-info control-button", style = "margin-top: 30px;")
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
    
    # Reactive calculation: Compute monthly schedule for both nominal and real balances.
    calculate_investment <- eventReactive(input$calculate, {
      initial <- input$initial
      monthly <- input$contribution
      rate_annual <- input$rate / 100
      monthly_rate <- rate_annual / 12
      years <- input$years
      months <- years * 12
      inflation_rate <- input$inflation / 100  # annual inflation rate
      
      # Calculate nominal balance using a simple loop
      nominal <- numeric(months)
      nominal[1] <- initial * (1 + monthly_rate) + monthly
      if(months > 1) {
        for (i in 2:months) {
          nominal[i] <- (nominal[i-1] + monthly) * (1 + monthly_rate)
        }
      }
      
      # Calculate real (inflation-adjusted) balance:
      # For each month, adjust nominal value by inflation for month/12 years.
      real <- nominal / ((1 + inflation_rate)^((1:months) / 12))
      
      data.frame(Month = 1:months, Nominal = nominal, Real = real)
    }, ignoreInit = FALSE, ignoreNULL = FALSE)
    
    # Nominal Growth Plot
    output$growthPlot_nominal <- renderPlotly({
      df <- calculate_investment()
      plot_ly(df, x = ~Month, y = ~Nominal, type = 'scatter', mode = 'lines',
              line = list(color = 'blue', width = 2)) %>%
        layout(title = list(text = "Nominal Investment Growth Over Time"),
               xaxis = list(title = "Months"),
               yaxis = list(title = "Balance (USD)"),
               margin = list(l = 50, r = 50, b = 50, t = 50))
    })
    
    # Inflation-Adjusted Growth Plot
    output$growthPlot_real <- renderPlotly({
      df <- calculate_investment()
      plot_ly(df, x = ~Month, y = ~Real, type = 'scatter', mode = 'lines',
              line = list(color = 'green', width = 2)) %>%
        layout(title = list(text = "Inflation-Adjusted Investment Growth Over Time"),
               xaxis = list(title = "Months"),
               yaxis = list(title = "Balance (USD)"),
               margin = list(l = 50, r = 50, b = 50, t = 50))
    })
    
    # Summary Table
    output$summaryTable <- renderDataTable({
      df <- calculate_investment()
      df$Nominal <- scales::dollar(df$Nominal)
      df$Real <- scales::dollar(df$Real)
      df
    })
    
    # Summary text and value box output
    output$investment_summary <- renderUI({
      df <- calculate_investment()
      final_nominal <- tail(df$Nominal, 1)
      final_real <- tail(df$Real, 1)
      
      HTML(paste0(
        "<div style='font-family: \"Nunito\", sans-serif; background-color: #f8f9fa; padding: 20px; border: 1px solid #ddd; border-radius: 8px; margin-bottom: 20px;'>",
          "<h3 style='margin-top: 0; color: #2c3e50;'>Investment Summary</h3>",
          "<p style='font-size: 16px; margin-bottom: 10px;'><strong>Future Value (Nominal):</strong> ", 
              scales::dollar(round(final_nominal, 0)), " per year</p>",
          "<p style='font-size: 16px; margin-bottom: 10px;'><strong>Inflation-Adjusted Value:</strong> ", 
              scales::dollar(round(final_real, 0)), " per year</p>",
          "<p style='font-size: 16px; margin-bottom: 0;'><strong>Goal:</strong> ", 
              scales::dollar(input$goal), "</p>",
        "</div>"
      ))
    })

    
    output$final_balance_box <- renderValueBox({
      df <- calculate_investment()
      final_nominal <- tail(df$Nominal, 1)
      box_color <- if(final_nominal >= input$goal) "success" else "warning"
      valueBox(
        scales::dollar(round(final_nominal, 0)),
        "Final Nominal Balance",
        icon = icon("chart-line"),
        color = box_color
      )
    })
    
    # Download Handler for Excel export of the schedule
    output$download_excel <- downloadHandler(
      filename = function() {
        paste("investment_schedule_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        df <- calculate_investment()
        writexl::write_xlsx(df, path = file)
      }
    )
  })
}
