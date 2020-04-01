#' @importFrom shiny uiOutput
#' @export
shiny_vector_filter_ui.numeric = function(data, inputId) {
  ns <- NS(inputId)
  shiny::uiOutput(ns("ui"))
}



#' @export
shiny_vector_filter.numeric <- function(data, inputId, ...) {
  n_vals <- length(unique(as.character(data)))
  if (n_vals > 10)
    shiny_vector_filter_numeric_many
  else
    shiny_vector_filter_numeric_few
}