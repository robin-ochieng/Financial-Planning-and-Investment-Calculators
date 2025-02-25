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
            class = "page-title",
          ),
          p(
            "Use this calculator to estimate your income replacement ratio at retirement. Enter your personal details, financial information, and investment assumptions to get started.",
            style = "margin-top: 10px;"
          )
        )
      )
    ),
    fluidRow(
      box(
        status = "success",
        title = "User Input", width = 6, height = "760px",
        textInput(ns("name"), "Full Name"),
        dateInput(ns("dob"), "Date of Birth", value = Sys.Date() - years(45)),
        numericInput(ns("retirement_age"), "Normal Retirement Age", value = 65, min = 40, max = 70),
        numericInput(ns("contribution_rate"), "Total Contribution Rate (%)", value = 25, min = 0, max = 100),
        numericInput(ns("salary"), "Current Monthly Salary", value = 250000, min = 0),
        numericInput(ns("investment_return"), "Investment Return (%)", value = 11, min = 0, max = 100),
        numericInput(ns("salary_escalation"), "Salary Escalation (%)", value = 3, min = 0, max = 100),
        numericInput(ns("fund_balance"), "Current Fund Balance", value = 8000000, min = 0),
        actionButton(ns("compute"), "Compute", class = "btn-primary control-button")
      ),
      box(
        title = "Results", status = "success", width = 6, height = "760px",
        fluidRow(
          style = "margin-bottom: 10px;", 
          # 1) Title text output
          div(style = "margin-bottom: 10px;", textOutput(ns("income_ratio_title"))),
        
          # 2) IRR value box
          valueBoxOutput(ns("income_ratio_box"), width = 12)  # You can split into multiple boxes if you prefer
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

# Module server function
irrCalcServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$compute, {
      # Calculate current age in years
      age <- as.numeric(difftime(Sys.Date(), input$dob, units = "weeks")) / 52.25
      years_to_retirement <- input$retirement_age - age
      
      # Calculate projected balance at retirement
      projected_balance <- input$fund_balance * (1 + input$investment_return / 100)^years_to_retirement +
        sum((input$salary * (1 + input$salary_escalation / 100)^(0:(years_to_retirement - 1))) *
              (input$contribution_rate / 100) * (1 + input$investment_return / 100)^(years_to_retirement:1))
      
      # Calculate annual pension and salary at retirement
      annual_pension <- projected_balance / 15
      annual_salary_at_retirement <- input$salary * (1 + input$salary_escalation / 100)^years_to_retirement * 12
      income_replacement_ratio <- (annual_pension / annual_salary_at_retirement) * 100
      
     #---------------------------
      # 1) IRR Title and Value Box
      #---------------------------
      output$income_ratio_title <- renderText({
        ratio_rounded <- round(income_replacement_ratio, 1)  # 1 decimal or as desired
        
        if (income_replacement_ratio < 60) {
          paste("Your Income Replacement Ratio is Too Low: ", paste0(ratio_rounded, "%"))
        } else if (income_replacement_ratio < 80) {
          paste("Your Income Replacement Ratio is Adequate: ", paste0(ratio_rounded, "%"))
        } else {
          paste("Your Income Replacement Ratio is Sufficient: ", paste0(ratio_rounded, "%"))
        }
      }) 

      # 2) IRR value box
      output$income_ratio_box <- renderValueBox({
        # Choose color logic based on ratio
        vb_color <- if (income_replacement_ratio < 60) {
          "danger"
        } else if (income_replacement_ratio < 80) {
          "warning"
        } else {
          "success"
        }

        valueBox(
          paste0(round(income_replacement_ratio, 0), "%"),  # Show 0 decimals
          "Income Replacement Ratio",
          icon = icon("percentage"),
          color = vb_color
        )
      })

      #---------------------------
      # 2) Financial List (as an unordered list)
      #---------------------------
      output$financial_list <- renderUI({
        tags$ul(
          tags$li(
            strong("Projected Balance at Retirement: "),
            paste("KES", formatC(projected_balance, format = "f", big.mark = ",", digits = 0))
          ),
          tags$li(
            strong("Annual Pension at Retirement: "),
            paste("KES", formatC(annual_pension, format = "f", big.mark = ",", digits = 0))
          ),
          tags$li(
            strong("Annual Salary at Retirement: "),
            paste("KES", formatC(annual_salary_at_retirement, format = "f", big.mark = ",", digits = 0))
          )
        )
      })

      #---------------------------
      # 3) Progress Bar
      #---------------------------
      output$progress_bar_ui <- renderUI({
        # Round ratio to integer for a clean display
        ratio_int <- round(income_replacement_ratio, 0)
        
        # Choose a Bootstrap color class based on ratio
        # "bg-danger" = red, "bg-warning" = orange, "bg-success" = green
        bar_color <- if (ratio_int < 60) {
          "bg-danger"
        } else if (ratio_int < 80) {
          "bg-warning"
        } else {
          "bg-success"
        }
        
        # Create the Bootstrap progress bar
        tags$div(
          class = "progress",
          style = "height: 30px; margin-bottom: 10px;",
          tags$div(
            class = paste("progress-bar", bar_color),
            role = "progressbar",
            style = paste0("width: ", max(min(ratio_int, 100), 0), "%;"),
            `aria-valuenow` = ratio_int,
            `aria-valuemin` = 0,
            `aria-valuemax` = 100,
            paste0(ratio_int, "%")
          )
        )
      })
      
      #---------------------------
      # 4) Progress Bar Message
      #---------------------------
      output$progress_bar_message <- renderUI({
        # Customize a user-friendly message
        # Example: "Below 60% = Just Retired, 60-80% = Adequate, 80-100% = Sufficient"
        tags$div(
        style = "font-style: italic; color: #555; margin-top: 10px;",
        tags$strong("Guideline:"),
        tags$ul(
            tags$li("Below 60% = 'Just Retired'"),
            tags$li("60-80% = 'Adequate'"),
            tags$li("80-100% = 'Sufficient'")
        )
        )
      })

      #---------------------------
      # 5) Disclaimer (displayed when Compute is clicked)
      #---------------------------
      output$disclaimer <- renderUI({
        tags$div(
          style = "background-color: #f9f9f9; border-radius: 5px; padding: 15px; margin-top: 20px;",
          h4("Disclaimer", style = "font-weight: bold; margin-bottom: 10px;"),
          p(
            style = "font-size: 14px; color: #333;",
            "Note: This calculator is provided as a guide and illustration. The values shown are based 
             on assumptions and your inputs. The values are shown in future terms and do not consider 
             the effect of inflation. Actual future values will be based on actual future experience 
             and will differ from these projections. We will not 
             be held responsible for your reliance on this calculator for your financial planning. 
             We recommend that you speak to a financial adviser and review your retirement plan 
             regularly to ensure you meet your goals."
          )
        )
      })

    }, ignoreInit = FALSE, ignoreNULL = FALSE)
    
  })
}
