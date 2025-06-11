# financialPlanningCalculator Module

# Define goal settings as a named list.
goalSettings <- list(
  "Education" = list(minAmount = 1e6, defaultTerm = 10, defaultAmount = 5e6),
  "Building a House" = list(minAmount = 1e6, defaultTerm = 5, defaultAmount = 1e7),
  "Retirement" = list(minAmount = 5e5, defaultTerm = 10, defaultAmount = 5e6),
  "Business/Start Up" = list(minAmount = 5e4, defaultTerm = 3, defaultAmount = 1e6),
  "Emergency Fund" = list(minAmount = 5e4, defaultTerm = 10, defaultAmount = 1e6),
  "Wedding/Dowry/Bride Price" = list(minAmount = 5e4, defaultTerm = 1, defaultAmount = 5e5),
  "Vehicle Purchase" = list(minAmount = 2.5e5, defaultTerm = 3, defaultAmount = 5e5),
  "Travel/Vacation" = list(minAmount = 5e4, defaultTerm = 1, defaultAmount = 2.5e5),
  "HealthCare Buffer" = list(minAmount = 5e4, defaultTerm = 1, defaultAmount = 2.5e5),
  "Graduation/Social Event" = list(minAmount = 5e4, defaultTerm = 0.25, defaultAmount = 5e4),
  "Personal Purchase e.g. Phone, Laptop" = list(minAmount = 5e4, defaultTerm = 0.5, defaultAmount = 5e4),
  "Investment" = list(minAmount = 1e5, defaultTerm = 3, defaultAmount = 1e6),
  "Other" = list(minAmount = 5e4, defaultTerm = 0.25, defaultAmount = 5e4)
)

# Module UI for the Financial Planning Calculator
financialPlanningCalcUI <- function(id) {
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
      column(
        width = 4,
        bs4Dash::tooltip(
          selectizeInput(
            inputId = ns("goals"),
            label = label_with_info(
              label_text = "Select Financial Goals:",
              info_id = ns("goal_info"),
              popover_title = "Select your Financial Goals",
              popover_content = "You can now pick one or more goals to plan for simultaneously."
            ),
            choices = names(goalSettings),
            selected = "Building a House",
            multiple = TRUE,
            options    = list(plugins = list('remove_button'))
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
        status = "success",
        width = 6,
        height = "500px",
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
        status = "success",
        width = 6,
        collapsible = TRUE,
        style = "height: 500px; overflow-y: auto;",
        # Goal Amount (auto defaults based on selected goal if desired)
          uiOutput(ns("goals_ui")),
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
     uiOutput(ns("disclaimer")), 
     fluidRow(
      bs4Card(
        title = "Results Summary", 
        status = "success", 
        width = 12,
        id = ns("ResultsSummary"),
        fluidRow(
          div(style = "margin-bottom: 10px;", uiOutput(ns("fp_summary")))
        )
      )
    ),
    # Graphs: Placed below the inputs/results
    uiOutput(ns("goalPlots")),
    uiOutput(ns("goalTables"))
  )
}


# Module Server for the Financial Planning Calculator
financialPlanningCalcServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$disclaimer <- renderUI({
      tags$div(
        style = paste0(
          "background-color: #f8f9fa; ",
          "border-left: 6px solid #00964B; ",
          "padding: 15px; ",
          "margin-top: 20px; ",
          "margin-bottom: 30px; ",
          "border-radius: 6px;"
        ),
        
        # Header row
        tags$div(
          style = "display: flex; align-items: center; margin-bottom: 10px;",
          tags$i(
            class = "fa fa-exclamation-circle",
            style = "font-size: 24px; margin-right: 8px; color: #00964B;"
          ),
          tags$h4("Disclaimer", style = "font-weight: bold; margin: 0;")
        ),
        
        # Body message
        tags$p(
          style = "font-size: 14px; color: #333; margin-bottom: 0;",
          "This tool is provided for informational and illustrative purposes only. The projections and calculations are based on user inputs and assumptions and may not reflect future financial conditions or personal circumstances. Please seek advice from a certified financial planner before making any financial decisions."
        )
      )
    })


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
    
    # (B) HELPER: Create a label with an info icon that shows a tooltip on hover
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


    output$goals_ui <- renderUI({
      req(input$goals)
      # for each goal, build a bs4Card with its amount & term inputs
      cards <- lapply(input$goals, function(g) {
        settings <- goalSettings[[g]]
        bs4Card(
          title = paste(g, "Settings"),
          status = "success",
          width = 12,
          collapsible = TRUE,
          autonumericInput(
            ns(paste0(g, "_amount")),
            label = paste0("Goal Amount (", g, ")"),
            value = settings$defaultAmount
          ),
          numericInput(
            ns(paste0(g, "_term")),
            label = paste0("Term (years) – ", g),
            value = settings$defaultTerm,
            min = settings$minAmount / settings$defaultAmount,
            step = 0.25
          )
        )
      })
      do.call(tagList, cards)
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
    
    observeEvent(input$update, {
      # Scroll to projection box
      shinyjs::runjs(
        sprintf(
          "document.getElementById('%s').scrollIntoView({behavior: 'smooth'});",
          ns("ResultsSummary")
         )
      )
    })    

    
    # Reactive: Financial Profile Calculation for multiple goals
    fpData <- eventReactive(input$update, {
      req(input$goals)  # ensure at least one goal is selected
      withProgress(message = "Calculating financial profile...", value = 0, {
        nGoals <- length(input$goals)
        # allocate 80% of the progress bar across goals, leave 20% for startup/finalizing
        perGoal <- 0.8 / nGoals
        
        results <- lapply(seq_along(input$goals), function(i) {
          g <- input$goals[i]
          incProgress(perGoal, detail = paste("Processing goal:", g))
          
          # clamp net worth & annual savings to >=0
          net_worth      <- pmax(input$savings - input$debt, 0)
          annual_savings <- pmax(input$income - (input$expenses * 12), 0)
          savings_rate   <- if (input$income > 0) (annual_savings / input$income) * 100 else 0
          
          # read this goal’s specific inputs
          amt    <- as.numeric(input[[paste0(g, "_amount")]])
          term   <- as.numeric(input[[paste0(g, "_term")]])
          r      <- input$exp_return / 100
          
          # future value
          fv_nominal <- amt * (1 + r)^term + 
                        annual_savings * (((1 + r)^term - 1) / r)
          
          # gap and required monthly
          gap         <- pmax(amt - fv_nominal, 0)
          req_monthly <- if (gap > 0) gap / (term * 12) else 0
          
          # emergency fund check stays same
          emergency_status <- 
            if (input$emergency >= input$expenses * 3) "Sufficient" else "Insufficient"
          
          list(
            goal           = g,
            net_worth      = net_worth,
            annual_savings = annual_savings,
            savings_rate   = savings_rate,
            fv_nominal     = fv_nominal,
            gap            = gap,
            req_monthly    = req_monthly,
            emergency_status = emergency_status
          )
        })
        
        incProgress(0.1, detail = "Finalizing results...")
        results
      })
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    
    # -----------------------------------------------------------------
    # E) RESULT SUMMARY
    # -----------------------------------------------------------------
    output$fp_summary <- renderUI({
      data <- fpData()
      cur  <- input$currency

      # build each card’s HTML
      cards <- lapply(data, function(res) {
        card_html <- paste0(
          "<div style='font-family: \"Nunito\", sans-serif; background-color: #f9f9f9;",
                      " padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1);",
                      " margin-bottom: 20px;'>",
            "<h3 style='margin-top:0; color:#2c3e50;'>", res$goal, " Summary</h3>",
            "<ul style='list-style:none; padding-left:0; font-size:16px; line-height:1.6; margin-bottom:20px;'>",
              "<li><strong>Net Worth:</strong> ",      formatCurrency(res$net_worth, cur),        "</li>",
              "<li><strong>Savings Rate:</strong> ",    sprintf("%.1f", res$savings_rate), "%</li>",
              "<li><strong>Future Value:</strong> ",    formatCurrency(res$fv_nominal, cur),       "</li>",
              "<li><strong>Monthly Shortfall:</strong> ", formatCurrency(res$req_monthly, cur),    "</li>",
              "<li><strong>Emergency Fund:</strong> ",  res$emergency_status,                      "</li>",
            "</ul>",
            if (res$gap > 0) {
              paste0(
                "<div style='font-size:16px; color:#d9534f;'>",
                  "<strong>Recommendation:</strong> Save an additional ",
                  formatCurrency(res$req_monthly, cur),
                  " per month to meet this goal within the chosen term.",
                "</div>"
              )
            } else {
              "<div style='font-size:16px; color:#5cb85c;'><strong>Recommendation:</strong> On track to meet this goal.</div>"
            },
          "</div>"
        )
        HTML(card_html)
      })
      # if only one card, make it full-width; otherwise two-column layout
      n <- length(cards)
      fluidRow(
        lapply(cards, function(card) {
          column(
            width = if (n == 1) 12 else 6,
            card
          )
        })
      )
    })

    # -------------- Dynamic Plot Outputs --------------
    output$goalPlots <- renderUI({
      req(fpData())
      req(input$goals)
      # one bs4Card + plot per goal
      cards <- lapply(input$goals, function(g) {
        bs4Card(
          title = paste0(g, " Projection (Nominal)"),
          status = "success",
          width = 12,
          plotlyOutput(ns(paste0(g, "_plot")), height = "300px")
        )
      })
      do.call(tagList, cards)
    })

    # -------------- Dynamic Table Outputs --------------
    output$goalTables <- renderUI({
      req(fpData())   
      req(input$goals)
      cards <- lapply(input$goals, function(g) {
        bs4Card(
          title = paste0(g, " Projection Schedule"),
          status = "success",
          width = 12,
          # position download button at top-right
          tags$div(
            style = "position: absolute; top: 15px; right: 15px; z-index: 1000;",
            downloadButton(ns(paste0(g, "_download")), "Download Table", class = "btn-sm btn-info")
          ),
          # leave space for the button so it doesn’t overlap the title
          tags$div(style="margin-top:30px;"),
          dataTableOutput(ns(paste0(g, "_table")))
        )
      })
      do.call(tagList, cards)
    })

    for (g in names(goalSettings)) {
      local({
        goalName <- g
            output[[paste0(goalName, "_plot")]] <- renderPlotly({
                  # build schedule for this goal
                  term  <- as.numeric(input[[paste0(goalName, "_term")]])
                  r     <- input$exp_return/100
                  pr    <- input$savings
                  sav   <- pmax(input$income - input$expenses*12, 0)
                  yrs   <- 0:term
                  nom   <- pr*(1+r)^yrs + sav*(((1+r)^yrs - 1)/r)
                  df    <- data.frame(Year=yrs, Nominal=nom)
                  plot_ly(df, x=~Year, y=~Nominal, type="scatter", mode="lines",
                          name=goalName) %>%
                    layout(title = goalName)
                })        
          # Table
          output[[paste0(goalName, "_table")]] <- renderDataTable({
            term  <- as.numeric(input[[paste0(goalName, "_term")]])
            r     <- input$exp_return/100
            pr    <- input$savings
            sav   <- pmax(input$income - input$expenses*12, 0)
            yrs   <- 0:term
            nom   <- pr*(1+r)^yrs + sav*(((1+r)^yrs - 1)/r)
            df    <- data.frame(
                      Year = yrs,
                      Nominal = nom,
                      Cumulative_Contributions = pr + sav*yrs,
                      Total_Interest = nom - (pr + sav*yrs)
                    )
            # Format currency columns
            df[] <- lapply(df, function(col) {
              if(is.numeric(col)) formatCurrency(col, input$currency) else col
            })
            df
          }, options = list(scrollX=TRUE, paging=FALSE))

          output[[paste0(goalName, "_download")]] <- downloadHandler(
            filename = function() {
              paste0(gsub(" ", "_", goalName), "_schedule_", Sys.Date(), ".xlsx")
            },
            content = function(file) {
              # rebuild the same schedule you render in the table
              term  <- as.numeric(input[[paste0(goalName, "_term")]])
              r     <- input$exp_return / 100
              pr    <- input$savings
              sav   <- pmax(input$income - input$expenses * 12, 0)
              yrs   <- 0:term
              nom   <- pr * (1+r)^yrs + sav * (((1+r)^yrs - 1) / r)
              df    <- data.frame(
                Year = yrs,
                Nominal = nom,
                Cumulative_Contributions = pr + sav * yrs,
                Total_Interest = nom - (pr + sav * yrs)
              )
              # write to Excel
              wb <- openxlsx::createWorkbook()
              openxlsx::addWorksheet(wb, "Schedule")
              openxlsx::writeData(wb, "Schedule", df)
              openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
            }
          )

      })
    }


  })
}