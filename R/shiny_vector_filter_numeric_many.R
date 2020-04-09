#' A vector filter for numeric variables with only many choices
#'
#' @importFrom shiny reactive reactiveValues renderUI div plotOutput sliderInput
#'   isolate tags validate need renderPlot
#' @importFrom ggplot2 ggplot aes aes_ geom_area theme_void scale_x_continuous
#'   scale_y_continuous
#' @importFrom grDevices rgb
#' @importFrom stats density
#' @export
shiny_vector_filter_numeric_many <- function(input, output, session, x = shiny::reactive(numeric()), 
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
          shiny::sliderInput(ns("param"), NULL,
                             value = shiny::isolate(input$param) %||% range(x(), na.rm = TRUE), 
                             min = min(round(x(), 1), na.rm = TRUE), 
                             max = max(round(x(), 1), na.rm = TRUE))
        } else {
          shiny::div(
            style = "padding-top: 10px; opacity: 0.3; text-align: center;",
            shiny::tags$h5(shiny::tags$i("no numeric values")))
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