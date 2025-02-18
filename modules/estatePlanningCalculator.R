# estatePlanningCalculator Module
# Module UI Function for the Estate Planning module
estatePlanningCalcUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        div(
          h2("Estate Planning Calculator", class = "page-title")
        )
      )
    ),
    # Row 1: Assets & Liabilities
    fluidRow(
      bs4Card(
        title = "Assets",
        status = "primary",
        width = 6,
        solidHeader = TRUE,
        collapsible = TRUE,
        numericInput(ns("real_estate"), "Real Estate (KES):", value = 10000000, min = 0, step = 100000),
        numericInput(ns("investments"), "Investments (KES):", value = 5000000, min = 0, step = 100000),
        numericInput(ns("bank_savings"), "Bank Savings (KES):", value = 3000000, min = 0, step = 100000),
        numericInput(ns("business"), "Business Interests (KES):", value = 8000000, min = 0, step = 100000),
        numericInput(ns("personal_property"), "Personal Property (KES):", value = 2000000, min = 0, step = 100000),
        numericInput(ns("other_assets"), "Other Assets (KES):", value = 1000000, min = 0, step = 100000)
      ),
      bs4Card(
        title = "Liabilities",
        status = "danger",
        width = 6,
        solidHeader = TRUE,
        collapsible = TRUE,
        numericInput(ns("mortgages"), "Mortgages (KES):", value = 4000000, min = 0, step = 100000),
        numericInput(ns("loans"), "Loans (KES):", value = 2000000, min = 0, step = 100000),
        numericInput(ns("credit_cards"), "Credit Card Debts (KES):", value = 200000, min = 0, step = 10000),
        numericInput(ns("other_liabilities"), "Other Liabilities (KES):", value = 500000, min = 0, step = 10000)
      )
    ),
    # Row 2: Deductions & Beneficiary Distribution
    fluidRow(
      bs4Card(
        title = "Deductions & Estate Duty",
        status = "warning",
        width = 6,
        solidHeader = TRUE,
        collapsible = TRUE,
        numericInput(ns("funeral_expenses"), "Funeral Expenses (KES):", value = 500000, min = 0, step = 10000),
        numericInput(ns("estate_duty_rate"), "Estate Duty Rate (%):", value = 25, min = 0, step = 0.1),
        numericInput(ns("estate_duty_exemption"), "Estate Duty Exemption (KES):", value = 5000000, min = 0, step = 100000),
        numericInput(ns("other_deductions"), "Other Deductions (KES):", value = 300000, min = 0, step = 10000)
      ),
      bs4Card(
        title = "Beneficiary Distribution (%)",
        status = "info",
        width = 6,
        solidHeader = TRUE,
        collapsible = TRUE,
        numericInput(ns("spouse_pct"), "Spouse (%):", value = 50, min = 0, max = 100, step = 1),
        numericInput(ns("children_pct"), "Children (%):", value = 30, min = 0, max = 100, step = 1),
        numericInput(ns("others_pct"), "Other Heirs (%):", value = 20, min = 0, max = 100, step = 1)
      )
    ),
    # Row 3: Calculate button
    fluidRow(
      column(
        width = 12,
        align = "center",
        actionButton(ns("calculate"), "Calculate Estate Plan", class = "btn-success")
      )
    ),
    # Row 4: Estate Summary Output
    fluidRow(
      bs4Card(
        title = "Estate Summary",
        status = "success",
        width = 12,
        solidHeader = TRUE,
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
  })
  
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
