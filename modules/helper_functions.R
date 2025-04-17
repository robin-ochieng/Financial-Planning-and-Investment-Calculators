# info_button <- function(id, title, content) {
#   bs4Dash::popover(
#     shiny::actionButton(
#       inputId = id,
#       label = NULL,
#       icon = icon("info-circle"),
#       style = "color: #007bff; margin-left: 5px; border: none; background: none;"
#     ),
#     title = title,
#     content = content,
#     placement = "right"
#   )
# }

label_with_info <- function(label_text, info_id, popover_title, popover_content) {
  tagList(
    tags$span(label_text),
    bs4Dash::popover(
      shiny::actionButton(
        inputId = info_id,
        label = NULL,
        icon = icon("info-circle"),
        style = "color: #007bff; margin-left: 5px; border: none; background: none;"
      ),
      title = popover_title,
      content = popover_content,
      placement = "right"
    )
  )
}




