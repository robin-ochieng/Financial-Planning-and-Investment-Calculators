library(shiny)
library(bs4Dash)
library(ggplot2)
library(bslib)

source("modules/retirementCalculator.R")
source("modules/irrCalculator.R")

# Define a custom theme using bslib
my_theme <- bs_theme(
  bg = "#202123", 
  fg = "#E1E1E1", 
  primary = "#EA80FC", 
  info = "#17a2b8",
  secondary = "#00BFA5",
  base_font = font_google("Mulish"),
  heading_font = font_google("Mulish"),
  code_font = font_google("Mulish"),
  navbar_bg = "#333333",  
  navbar_fg = "#ffffff"  
)


# Define the UI
ui <- dashboardPage(
  title = "Financial & Retirement Planning Workbench",
  freshTheme = my_theme,
  dark = NULL,
  help = NULL,
  fullscreen = FALSE,
  scrollToTop = TRUE,
  dashboardHeader(
    fixed = TRUE,
    sidebarIcon = NULL,
    title = dashboardBrand(
      title = tags$div(
        class = "text-center header-title-container",
        tags$h4("Financial & Retirement Planning Workbench", class = "header-title")
      )
    ),
    tags$li(
      class = "clock-container",
    tags$span(
      id = "dynamic-clock"
      ),
    )
  ),
  dashboardSidebar(
    tags$div(
      class = "menu-container",
    sidebarMenu(
      menuItem("Retirement Calculator", tabName = "retirementCalculator", icon = icon("user")),
      menuItem("IRR Calculator", tabName = "irrCalculator", icon = icon("user"))
    )
   )
  ),
  dashboardBody(
    tags$head(
      includeCSS("www/css/custom_styles.css"),      
      tags$script(src = "js/custom.js"),
      tags$link(rel = "shortcut icon", href = "favicon/kenbright.ico", type = "image/x-icon"),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Nunito:wght@400;700&display=swap", 
        rel = "stylesheet")
    ),
    tabItems(
      tabItem(tabName = "retirementCalculator",
              # Call the module UI with an ID (e.g., "retirement")
              retirementCalcUI("retirement")
      ),
      tabItem(tabName = "irrCalculator",
              # Call the module UI with an ID (e.g., "irr")
              
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Call the module server with an ID (e.g., "retirement")
  retirementCalcServer("retirement")
}

# Run the Shiny app
shinyApp(ui, server)
