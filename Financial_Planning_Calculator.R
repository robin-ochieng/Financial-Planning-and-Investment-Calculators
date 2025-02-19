# Load required libraries
library(shiny)
library(bs4Dash)
library(ggplot2)
library(scales)

# Define the UI using bs4Dash
ui <- bs4DashPage(
  title = "Financial Planning Calculator",
  header = bs4DashNavbar(
    title = "Financial Planning Calculator"
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    bs4SidebarMenu(
      bs4SidebarMenuItem("Projections", tabName = "projections", icon = icon("chart-bar"))
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(
        tabName = "projections",
        # Input parameters in a collapsible card
        fluidRow(
          bs4Card(
            title = "Input Parameters",
            status = "primary",
            width = 12,
            collapsible = TRUE,
            fluidRow(
              column(width = 6,
                     h4("Accumulation Phase Inputs"),
                     numericInput("initial", "Initial Investment (KES):", value = 1000000, min = 0, step = 10000),
                     numericInput("annual", "Annual Contribution (KES):", value = 500000, min = 0, step = 10000),
                     numericInput("accYears", "Years to Retirement:", value = 30, min = 1, step = 1),
                     numericInput("accRate", "Annual Return Rate (%):", value = 10, min = 0, step = 0.1),
                     numericInput("accInflation", "Annual Inflation Rate (%):", value = 5, min = 0, step = 0.1)
              ),
              column(width = 6,
                     h4("Withdrawal Phase Inputs"),
                     numericInput("retPortfolio", "Retirement Portfolio (KES):", value = 5000000, min = 0, step = 10000),
                     numericInput("withdrawal", "Initial Annual Withdrawal (KES):", value = 400000, min = 0, step = 10000),
                     numericInput("retYears", "Years in Retirement:", value = 25, min = 1, step = 1),
                     numericInput("retRate", "Annual Return Rate in Retirement (%):", value = 8, min = 0, step = 0.1),
                     numericInput("retInflation", "Annual Inflation Rate (%):", value = 5, min = 0, step = 0.1)
              )
            ),
            fluidRow(
              column(width = 12, align = "center",
                     actionButton("update", "Update Projections", class = "btn-primary")
              )
            )
          )
        ),
        # Output: Two side-by-side cards for Accumulation and Withdrawal projections
        fluidRow(
          column(width = 6,
                 bs4Card(
                   title = "Accumulation Phase",
                   status = "info",
                   width = 12,
                   tabsetPanel(
                     tabPanel("Plot", plotOutput("accPlot", height = "400px")),
                     tabPanel("Data Table", tableOutput("accTable"))
                   )
                 )
          ),
          column(width = 6,
                 bs4Card(
                   title = "Withdrawal Phase",
                   status = "info",
                   width = 12,
                   tabsetPanel(
                     tabPanel("Plot", plotOutput("retPlot", height = "400px")),
                     tabPanel("Data Table", tableOutput("retTable"))
                   )
                 )
          )
        )
      )
    )
  )
)

# Define the server logic for the app
server <- function(input, output) {
  
  ### Accumulation Phase Calculations ###
  accData <- eventReactive(input$update, {
    years <- 0:input$accYears
    nominal <- numeric(length(years))
    real <- numeric(length(years))
    
    # Set starting values
    nominal[1] <- input$initial
    real[1] <- input$initial
    
    # Convert rates from percentage to decimal
    r <- input$accRate / 100
    inflation <- input$accInflation / 100
    contribution <- input$annual
    
    # Calculate portfolio values over time
    for (i in 2:length(years)) {
      nominal[i] <- nominal[i - 1] * (1 + r) + contribution
      real[i] <- nominal[i] / ((1 + inflation)^(years[i]))
    }
    
    data.frame(Year = years, Nominal = nominal, Real = real)
  }, ignoreNULL = FALSE)
  
  output$accPlot <- renderPlot({
    df <- accData()
    ggplot(df, aes(x = Year)) +
      geom_line(aes(y = Nominal, color = "Nominal Value (KES)"), size = 1.2) +
      geom_line(aes(y = Real, color = "Real Value (KES, Adjusted)"), size = 1.2, linetype = "dashed") +
      scale_y_continuous(labels = comma_format(accuracy = 1)) +
      labs(title = "Accumulation Phase Projection", y = "Portfolio Value (KES)", color = "Legend") +
      theme_minimal()
  })
  
  output$accTable <- renderTable({
    df <- accData()
    data.frame(
      Year = df$Year, 
      Nominal = format(df$Nominal, big.mark = ",", scientific = FALSE, trim = TRUE, digits = 1, nsmall = 0),
      Real = format(df$Real, big.mark = ",", scientific = FALSE, trim = TRUE, digits = 1, nsmall = 0)
    )
  })
  
  ### Withdrawal Phase Calculations ###
  retData <- eventReactive(input$update, {
    years <- 0:input$retYears
    portfolio <- numeric(length(years))
    withdrawal <- numeric(length(years))
    
    portfolio[1] <- input$retPortfolio
    withdrawal[1] <- input$withdrawal
    
    r <- input$retRate / 100
    inflation <- input$retInflation / 100
    
    for (i in 2:length(years)) {
      withdrawal[i] <- withdrawal[i - 1] * (1 + inflation)
      portfolio[i] <- portfolio[i - 1] * (1 + r) - withdrawal[i - 1]
    }
    
    data.frame(Year = years, Portfolio = portfolio, Withdrawal = withdrawal)
  }, ignoreNULL = FALSE)
  
  output$retPlot <- renderPlot({
    df <- retData()
    ggplot(df, aes(x = Year)) +
      geom_line(aes(y = Portfolio, color = "Portfolio Balance (KES)"), size = 1.2) +
      geom_line(aes(y = Withdrawal, color = "Annual Withdrawal (KES)"), size = 1.2, linetype = "dashed") +
      scale_y_continuous(labels = comma_format(accuracy = 1)) +
      labs(title = "Withdrawal Phase Projection", y = "Amount (KES)", color = "Legend") +
      theme_minimal()
  })
  
  output$retTable <- renderTable({
    df <- retData()
    data.frame(
      Year = df$Year, 
      Portfolio = format(df$Portfolio, big.mark = ",", scientific = FALSE, trim = TRUE, digits = 1, nsmall = 0),
      Withdrawal = format(df$Withdrawal, big.mark = ",", scientific = FALSE, trim = TRUE, digits = 1, nsmall = 0)
    )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
