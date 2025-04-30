#' Helper functions for the app
# info button function for use in labels 
label_with_info <- function(label_text, info_id, popover_title, popover_content) {
  htmltools::tagList(
    tags$span(label_text),
    bs4Dash::popover(
      shiny::actionLink(
        inputId = info_id,
        label = NULL,
        icon = icon("info-circle"),
        class   = "info-icon",
        tabindex = 0
      ),
      title = popover_title,
      content = popover_content,
      placement = "right"
    )
  )
}
