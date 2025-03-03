# IRR Calculator Module

# Module UI with proper namespacing
irrCalcUI <- function(id) {
  ns <- NS(id)
  tagList(
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
        title = "User Input", width = 6, height = "700px",
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
          numericInput(ns("salary"), "Current Monthly Salary", value = 250000, min = 0),
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
          numericInput(ns("fund_balance"), "Current Fund Balance", value = 8000000, min = 0),
          title = "Enter the current balance of your retirement fund.",
          placement = "right"
        )
      ),
      box(
        status = "success",
        title = "User Input", width = 6, height = "700px",
        bs4Dash::tooltip(
          numericInput(ns("social_security"), "Social Security (USD/year)", value = 20000, min = 0),
          title = "Enter your expected annual Social Security benefit in USD.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("pension_income"), "Pension (USD/year)", value = 10000, min = 0),
          title = "Enter your expected annual pension income in USD.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          numericInput(ns("savings_withdrawal"), "Retirement Savings Withdrawal (USD/year)", value = 25000, min = 0),
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
        title = "Results", status = "success", width = 12, height = "650px",
        fluidRow(
          style = "margin-bottom: 10px;", 
          # 1) Title text output
          div(style = "margin-bottom: 10px;", textOutput(ns("income_ratio_title")))
        ),
        fluidRow(
          valueBoxOutput(ns("income_ratio_box"), width = 12)
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

    observeEvent(input$compute, {
      # Use the provided current age directly
      age <- input$current_age
      years_to_retirement <- input$retirement_age - age
      
      # Annual pre-retirement income computed from monthly salary
      annual_income <- input$salary * 12
      
      # Desired income replacement value (in USD/year)
      desired_IRR_value <- annual_income * (input$desired_IRR / 100)
      
      # Total Retirement Income streams (USD/year)
      total_ret_income <- input$social_security + input$pension_income + input$savings_withdrawal
      
      # Calculate taxes on the taxable portion (pension and savings withdrawal)
      taxable_income <- input$pension_income + input$savings_withdrawal
      taxes <- taxable_income * (input$tax_rate / 100)
      after_tax_income <- total_ret_income - taxes
      
      # Adjust for inflation over the years until retirement
      inflation_factor <- (1 + input$inflation_rate / 100)^years_to_retirement
      desired_IRR_value_adj <- desired_IRR_value * inflation_factor
      after_tax_income_adj <- after_tax_income * inflation_factor
      
      # Determine shortfall (if any)
      shortfall <- desired_IRR_value_adj - after_tax_income_adj
      
      # ---------------------------
      # 1) IRR Title and Value Box
      # ---------------------------
      output$income_ratio_title <- renderText({
        # Use rounded values for display
        desired_disp <- dollar(round(desired_IRR_value_adj, 0))
        after_tax_disp <- dollar(round(after_tax_income_adj, 0))
        shortfall_disp <- dollar(round(shortfall, 0))
        
        if (shortfall > 0) {
          paste0(input$name, ", your projected retirement income (", after_tax_disp, 
                 "/year) is below your desired replacement (", desired_disp, 
                 "/year). \n
                 Shortfall: ", shortfall_disp)
        } else {
          paste0(input$name, ", your projected retirement income (", after_tax_disp, 
                 "/year) meets or exceeds your desired replacement (", desired_disp, 
                 "/year).")
        }
      })
      
      output$income_ratio_box <- renderValueBox({
        # Choose box color based on sufficiency
        vb_color <- if (shortfall > 0) {
          "danger"
        } else {
          "success"
        }
        
        valueBox(
          paste0(dollar(round(after_tax_income_adj, 0))), 
          "After-Tax Retirement Income",
          icon = icon("dollar-sign"),
          color = vb_color
        )
      })
      
      # ---------------------------
      # 2) Financial List (as an unordered list)
      # ---------------------------
      output$financial_list <- renderUI({
        tags$ul(
          tags$li(
            strong("Total Retirement Income: "),
            paste(dollar(total_ret_income), "/year")
          ),
          tags$li(
            strong("After-Tax Income: "),
            paste(dollar(after_tax_income), "/year")
          ),
          tags$li(
            strong("Desired IRR (Annual Replacement): "),
            paste(dollar(desired_IRR_value), "/year")
          ),
          tags$li(
            strong("Inflation Factor (over ", years_to_retirement, " years): "),
            round(inflation_factor, 2)
          ),
          tags$li(
            strong("Shortfall: "),
            paste(dollar(shortfall), "/year")
          )
        )
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
          style = "height: 30px; margin-bottom: 10px;",
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
      
    }, ignoreInit = FALSE, ignoreNULL = FALSE)
    
  })
}