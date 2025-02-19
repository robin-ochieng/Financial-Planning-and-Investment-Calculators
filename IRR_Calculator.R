library(shiny)
library(bs4Dash)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)

ui <- dashboardPage(
  dashboardHeader(title = "Income Replacement Ratio Calculator"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(title = "User Input", status = "primary", solidHeader = TRUE, width = 12,
          textInput("name", "Full Name"),
          dateInput("dob", "Date of Birth", value = Sys.Date() - years(18)),
          numericInput("retirement_age", "Normal Retirement Age", value = 50, min = 40, max = 70),
          numericInput("contribution_rate", "Total Contribution Rate (%)", value = 12, min = 0, max = 100),
          numericInput("salary", "Current Monthly Salary", value = 190000, min = 0),
          numericInput("investment_return", "Investment Return (%)", value = 10, min = 0, max = 100),
          numericInput("salary_escalation", "Salary Escalation (%)", value = 7, min = 0, max = 100),
          numericInput("fund_balance", "Current Fund Balance", value = 0, min = 0),
          actionButton("compute", "Compute", class = "btn-success")
      )
    ),
    fluidRow(
      box(title = "Results", status = "warning", solidHeader = TRUE, width = 12,
          textOutput("income_ratio"),
          plotlyOutput("progress_bar"),
          dataTableOutput("financial_projection")
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$compute, {
    age <- as.numeric(difftime(Sys.Date(), input$dob, units = "weeks")) / 52.25
    years_to_retirement <- input$retirement_age - age
    
    projected_balance <- input$fund_balance * (1 + input$investment_return / 100)^years_to_retirement + 
      sum((input$salary * (1 + input$salary_escalation / 100)^(0:(years_to_retirement - 1))) * 
            (input$contribution_rate / 100) * (1 + input$investment_return / 100)^(years_to_retirement:1))
    
    annual_pension <- projected_balance / 15
    annual_salary_at_retirement <- input$salary * (1 + input$salary_escalation / 100)^years_to_retirement * 12
    income_replacement_ratio <- (annual_pension / annual_salary_at_retirement) * 100
    
    output$income_ratio <- renderText({
      paste("Your Income Replacement Ratio is:", round(income_replacement_ratio, 2), "%")
    })
    
    output$progress_bar <- renderPlotly({
      plot_ly(x = c("Replacement Ratio"), y = c(income_replacement_ratio), type = "bar", marker = list(color = ifelse(income_replacement_ratio < 60, "red", ifelse(income_replacement_ratio < 80, "orange", "green"))))
    })
    
    output$financial_projection <- renderDataTable({
      data.frame(
        "Projected Balance at Retirement" = projected_balance,
        "Annual Pension at Retirement" = annual_pension,
        "Annual Salary at Retirement" = annual_salary_at_retirement
      )
    })
  })
}

shinyApp(ui, server)
