# modules/retirementCalculatorModule.R

retirementCalcUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Personal Details",
        textInput(ns("current_age"), "Your current age", "35"),
        textInput(ns("retirement_age"), "Planned retirement age", "67"),
        textInput(ns("life_expectancy"), "Life expectancy", "85"),
        textInput(ns("pre_tax_income"), "Current pre-tax income (KES)", "70000"), 
        width = 4, height = "400px"
      ),
      box(
        title = "Financial Details",
        textInput(ns("income_growth"), "Annual income increase (%)", "3"),
        textInput(ns("income_needed"), "Income needed after retirement (%)", "75"),
        textInput(ns("investment_return"), "Average investment return (%)", "6"),
        textInput(ns("inflation_rate"), "Inflation rate (%)", "3"), 
        width = 4, height = "400px"
      ),
      box(
        title = "Savings Details",
        textInput(ns("other_income"), "Other income after retirement (KES/month)", "0"),
        textInput(ns("current_savings"), "Current retirement savings (KES)", "30000"),
        textInput(ns("future_savings"), "Future savings (% of income)", "10"), 
        width = 4, height = "400px"
      )
    ),
    # Action button and plot output also use the namespace
    actionButton(ns("calculate"), "Calculate", class = "btn-primary"),
    plotOutput(ns("savingsPlot"))
  )
}

retirementCalcServer <- function(id) {
  moduleServer(id, function(input, output, session) {

  output$savingsPlot <- renderPlot({
    # Convert input values to numeric
    current_age <- as.numeric(input$current_age)
    retirement_age <- as.numeric(input$retirement_age)
    current_savings <- as.numeric(input$current_savings)
    future_savings <- as.numeric(input$future_savings) / 100  # Convert percentage to decimal
    income <- as.numeric(input$pre_tax_income)
    income_growth <- as.numeric(input$income_growth) / 100  # Convert percentage to decimal
    investment_return <- as.numeric(input$investment_return) / 100  # Convert percentage to decimal
    years <- current_age:retirement_age  # Sequence of years until retirement
    
    # Initialize savings array
    savings <- numeric(length(years))
    savings[1] <- current_savings  # Start with current savings
    
    # Calculate savings for each year
    for (i in 2:length(years)) {
      income <- income * (1 + income_growth)  # Increase income annually
      savings[i] <- savings[i - 1] * (1 + investment_return) + (income * future_savings)  # Compound growth + new savings
    }
    
    # Create a data frame for plotting
    df <- data.frame(Age = years, Savings = savings)
    
    # Generate the savings projection plot
    ggplot(df, aes(x = Age, y = Savings)) +
      geom_line(color = "blue", size = 1.2) +
      labs(title = "Retirement Savings Projection", x = "Age", y = "Savings (KES)") +
      theme_minimal()
  })
    

    
  })
}
