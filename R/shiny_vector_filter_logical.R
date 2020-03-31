#' @importFrom shiny NS uiOutput
#' @export
shiny_vector_filter_ui.logical <- function(data, inputId) {
  ns <- shiny::NS(inputId)
  shiny::uiOutput(ns("ui"))
}



#' @export
shiny_vector_filter.logical <- function(data, inputId, ...) {
  function(input, output, session, 
           x = shiny::reactive(logical()), filter_na = shiny::reactive(TRUE), 
           verbose = FALSE) {
    
    ns <- session$ns
    
    x_wo_NA <- shiny::reactive(Filter(Negate(is.na), x()))
    module_return <- shiny::reactiveValues(code = TRUE, mask = TRUE)
    choices <- shiny::reactive({
      Filter(function(i) i %in% x(), c("True" = TRUE, "False" = FALSE))
    })
    
    output$ui <- shiny::renderUI({
      filter_log("updating ui", verbose = verbose)
      shiny::div(style = "position: relative;",
                 shiny::div(style = "
          position: absolute; 
          top: -2px; right: 16px; bottom: -2px; left: 16px;
          animation: 
            0.75s ease-out 0s 1 shinyDataFilterEnlargeX, 
            0.5s ease-in  0s 1 shinyDataFilterFadeIn; 
          transform-origin: left;",
                            shiny::plotOutput(ns("plot"), height = "100%")),
                 shiny::checkboxGroupInput(ns("param"), NULL,
                                           choices = choices(),
                                           selected = shiny::isolate(input$param) %||% c(),
                                           width = "100%"))
    })
    
    module_return$code <- shiny::reactive({
      exprs <- list()
      
      if (TRUE %in% input$param) 
        exprs <- append(exprs, list(quote(.x)))
      if (FALSE %in% input$param) 
        exprs <- append(exprs, list(quote(!.x)))
      
      if (length(input$param) == 2 && filter_na())
        exprs <- list(quote(!is.na(.x)))
      else if (length(input$param) && !filter_na()) 
        exprs <- append(exprs, list(quote(is.na(.x))))
      
      
      if (length(exprs) && length(exprs) < 3) 
        Reduce(function(l, r) bquote(.(l) | .(r)), exprs)
      else 
        TRUE
    })
    
    module_return$mask <- shiny::reactive({
      eval(do.call(substitute, list(module_return$code(), list(.x = x()))))
    })
    
    module_return
  }
}