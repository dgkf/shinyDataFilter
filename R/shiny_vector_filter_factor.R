#' @importFrom shiny NS uiOutput
#' @export
shiny_vector_filter_ui.factor <- function(data, inputId) {
  ns <- shiny::NS(inputId)
  shiny::uiOutput(ns("ui"))
}



#' @export
shiny_vector_filter.factor <- function(data, inputId, ...) {
  if (length(unique(as.character(data))) > 5)
    shiny_vector_filter_factor_many
  else
    shiny_vector_filter_factor_few
}