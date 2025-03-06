# estatePlanningCalculator Module
# Module UI Function for the Estate Planning module
estatePlanningCalcUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        div(
          h2("Estate Planning Calculator", class = "page-title"),
          p("Use this calculator to estimate the value of your estate, calculate estate duty, and distribute your assets among beneficiaries. Enter your assets, liabilities, deductions, and beneficiary distribution to get started.",
            style = "margin-top: 10px;")
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
                          label = "Real Estate (KES):",  
                          value = 10000000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the current market value of your real estate holdings.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("investments"),  
                          label = "Investments (KES):",  
                          value = 5000000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the value of your investment assets such as stocks, bonds, and mutual funds.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("bank_savings"),  
                          label = "Bank Savings (KES):",  
                          value = 3000000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the amount in your bank savings accounts.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("business"),  
                          label = "Business Interests (KES):",  
                          value = 8000000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the estimated value of your business interests.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("personal_property"),  
                          label = "Personal Property (KES):",  
                          value = 2000000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the value of personal assets such as vehicles, jewelry, etc.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("other_assets"),  
                          label = "Other Assets (KES):",  
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
                          label = "Mortgages (KES):",  
                          value = 4000000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the outstanding balance of your mortgages.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("loans"),  
                          label = "Loans (KES):",  
                          value = 2000000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the total amount of your personal loans.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("credit_cards"),  
                          label = "Credit Card Debts (KES):",  
                          value = 200000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the total outstanding balance on your credit cards.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("other_liabilities"),  
                          label = "Other Liabilities (KES):",  
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
                          label = "Funeral Expenses (KES):",  
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
                          label = "Estate Duty Exemption (KES):",  
                          value = 5000000, 
                          decimalPlaces = 0, 
                          digitGroupSeparator = ","),
          title = "Enter the exemption amount for estate duty.",
          placement = "right"
        ),
        bs4Dash::tooltip(
          autonumericInput(inputId = ns("other_deductions"),  
                          label = "Other Deductions (KES):",  
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
        tableOutput(ns("summaryTable")),
        uiOutput(ns("distributionWarning"))
      )
    )
  )
}



# Module server function
estatePlanningCalcServer <- function(id) {
  moduleServer(id, function(input, output, session) {
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
    summary_df <- data.frame(
      Item = c("Total Assets", "Total Liabilities", "Preliminary Estate", 
               "Estate Duty", "Additional Deductions", "Final Estate",
               "Spouse Distribution", "Children Distribution", "Other Heirs Distribution"),
      Amount_KES = c(total_assets, total_liabilities, preliminary_estate, 
                     estate_duty, additional_deductions, final_estate,
                     spouse_distribution, children_distribution, others_distribution)
    )
    
    # Format amounts with commas and no decimals
    summary_df$Amount_KES <- format(round(summary_df$Amount_KES, 0), big.mark = ",", scientific = FALSE)
    
    list(summary = summary_df, total_pct = total_pct)
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  
  # Render the summary table
  output$summaryTable <- renderTable({
    estatePlan()$summary
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  # Display a warning if beneficiary percentages don't sum to 100
  output$distributionWarning <- renderUI({
    total_pct <- estatePlan()$total_pct
    if(total_pct != 100){
      warning_text <- paste0("Warning: The beneficiary distribution percentages sum to ", total_pct, 
                             "%. They have been scaled proportionally to total 100%.")
      div(style = "color: red; font-weight: bold;", warning_text)
    } else {
      NULL
     }
   })
 })
}
