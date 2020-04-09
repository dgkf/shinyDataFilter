#' @importFrom shiny NS uiOutput
#' @export
shiny_vector_filter_ui.character <- function(data, inputId) {
  ns <- shiny::NS(inputId)
  shiny::uiOutput(ns("ui"))
}

#' @importFrom shiny reactive reactiveValues renderUI textInput isolate
#' @export
shiny_vector_filter.character <- function(data, inputId, ...) {
    function(input, output, session, x = shiny::reactive(character()), 
             filter_na = shiny::reactive(FALSE), verbose = FALSE) {
  
  ns <- session$ns
  
  #x = unique(x())
  x_wo_NAs <- shiny::reactive(Filter(Negate(is.na), x()))
  
  module_return <- shiny::reactiveValues(code = TRUE, mask = TRUE)
  
  output$ui <- shiny::renderUI({
    
    filter_log("updating ui", verbose = verbose)
    
    if (map(x(), is.empty) %>% reduce(`&`)) {
      shiny::div(style = "opacity: 0.5;",
                 p(width = "100%", 
                   align = "center", 
                   "Input is blank"))
    } else {
      proportionSelectInput(ns("param"), NULL,
                            vec = x,
                            selected = shiny::isolate(input$param) %||% c(),
                            multiple = TRUE,
                            width = "100%")
    }

  })
  
  module_return$code <- shiny::reactive({
    if (length(input$param))
      bquote(.x %in% .(c(if (filter_na()) c() else NA, input$param)))
    else if (filter_na())
      bquote(!is.na(.x))
    else
      TRUE
  })
  
  module_return$mask <- shiny::reactive({
    eval(do.call(substitute, list(module_return$code(), list(.x = x()))))
  })
  module_return
    }
}