library(shiny)
library(bs4Dash)
library(DT)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = "Investment Calculator (KES)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calculator", tabName = "calculator", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "calculator",
              fluidRow(
                box(title = "Investment Inputs", status = "primary", solidHeader = TRUE, width = 4,
                    numericInput("initial", "Initial Investment (KES):", value = 100000, min = 0, step = 1000),
                    numericInput("contribution", "Monthly Contribution (KES):", value = 10000, min = 0, step = 1000),
                    numericInput("rate", "Annual Interest Rate (%):", value = 8, min = 0, step = 0.1),
                    numericInput("years", "Investment Duration (years):", value = 10, min = 1, step = 1),
                    actionButton("calculate", "Calculate", class = "btn-success")
                ),
                box(title = "Results", status = "success", solidHeader = TRUE, width = 8,
                    plotOutput("growthPlot")
                ),
                box(title = "Summary Table", status = "success", solidHeader = TRUE, width = 12,
                    dataTableOutput("summaryTable")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  calculate_investment <- eventReactive(input$calculate, {
    initial <- input$initial
    monthly <- input$contribution
    rate <- input$rate / 100 / 12
    months <- input$years * 12
    
    balance <- numeric(months)
    balance[1] <- initial * (1 + rate)
    
    for (i in 2:months) {
      balance[i] <- (balance[i-1] + monthly) * (1 + rate)
    }
    
    data.frame(Month = 1:months, Balance = balance) %>%
      mutate(Balance = paste0("KES ", formatC(Balance, format = "f", big.mark = ",", digits = 0)))
  }, ignoreNULL = FALSE)
  
  output$growthPlot <- renderPlot({
    df <- calculate_investment()
    df$Balance <- as.numeric(gsub("KES ", "", gsub(",", "", df$Balance)))
    ggplot(df, aes(x = Month, y = Balance)) +
      geom_line(color = "blue", size = 1) +
      labs(title = "Investment Growth Over Time", x = "Months", y = "Balance (KES)") +
      theme_minimal()
  })
  
  output$summaryTable <- renderDataTable({
    df <- calculate_investment()
    datatable(df, options = list(pageLength = 10))
  })
}

shinyApp(ui, server)