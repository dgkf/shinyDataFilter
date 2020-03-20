#' @importFrom shiny div
#' @export
shiny_vector_filter_ui.NULL = function(data, inputId) {
  shiny::div()
}


#' @importFrom shiny reactive reactiveValues
#' @export
shiny_vector_filter.NULL <- function(data, inputId, ...) {
  function(input, output, session, x = shiny::reactive(NULL), 
    filter_na = shiny::reactive(FALSE), verbose = FALSE) { 
    
    module_return <- shiny::reactiveValues(code = TRUE, mask = TRUE)
    module_return$code <- shiny::reactive(TRUE)
    module_return$mask <- shiny::reactive(TRUE) 
    
    module_return
  }
}