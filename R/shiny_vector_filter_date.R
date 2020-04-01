#' @importFrom shiny NS uiOutput
#' @export
shiny_vector_filter_ui.Date <- function(data, inputId) {
  ns <- shiny::NS(inputId)
  shiny::uiOutput(ns("ui"))
}



#' @export
shiny_vector_filter.Date <- function(input, output, session, x = shiny::reactive(lubridate::Date()), 
                                     filter_na = shiny::reactive(FALSE), verbose = FALSE) {
  
  ns <- session$ns
  module_return <- shiny::reactiveValues(code = TRUE, mask = TRUE)
  
  output$ui <- shiny::renderUI({
    filter_log("updating ui", verbose = verbose)
    shiny::div(
      shiny::div(style = "
                   margin: 0px 11px -25px 11px;
                   height: 25px;
                   animation: 
                   0.75s ease-out 0s 1 shinyDataFilterEnlargeY, 
                   0.5s ease-in  0s 1 shinyDataFilterFadeIn; 
                   transform-origin: bottom;"),
      if (any(!is.na(x()))) {
        shiny::dateRangeInput(ns("param"), NULL,
                           start = min(x(), na.rm = TRUE), 
                           end = max(x(), na.rm = TRUE))
      } else {
        shiny::div(
          style = "padding-top: 10px; opacity: 0.3; text-align: center;",
          shiny::tags$h5(shiny::tags$i("no Date values")))
      })
  })
  
  module_return$code <- shiny::reactive({
    exprs <- list()
    
    if (!is.null(input$param)) {
      if (input$param[[1]] > min(x(), na.rm = TRUE))
        exprs <- append(exprs, bquote(.x >= .(as.numeric(input$param[[1]]))))
      if (input$param[[2]] < max(x(), na.rm = TRUE))
        exprs <- append(exprs, bquote(.x <= .(as.numeric(input$param[[2]]))))
    }
    
    if (length(exprs) > 1) {
      expr <- Reduce(function(l, r) bquote(.(l) & .(r)), exprs)
      if (!filter_na()) expr <- bquote(is.na(.x) | (.(expr)))
    } else if (length(exprs) == 1) {
      expr <- exprs[[1]]
      if (!filter_na()) expr <- bquote(is.na(.x) | .(expr))
    } else if (filter_na()) {
      expr <- bquote(!is.na(.x))
    } else {
      return(TRUE)
    }
    
    expr
  })
  
  module_return$mask <- shiny::reactive({
    eval(do.call(substitute, list(module_return$code(), list(.x = x()))))
  })
  
  module_return
}