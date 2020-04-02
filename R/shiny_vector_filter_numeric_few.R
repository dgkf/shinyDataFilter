
#' A vector filter for numeric variables with only a few choices
#'
#' @importFrom shiny reactive reactiveValues renderUI div plotOutput sliderInput
#'   isolate tags validate need renderPlot
#' @importFrom ggplot2 ggplot aes aes_ geom_area theme_void scale_x_continuous
#'   scale_y_continuous
#' @importFrom grDevices rgb
#' @importFrom stats density
#' @export
shiny_vector_filter_numeric_few <- function(input, output, session,
            x = shiny::reactive(factor()),  #important: changed x to factor here
           filter_na = shiny::reactive(FALSE), verbose = FALSE) {
    
  ns <- session$ns
  
  x_wo_NA <- shiny::reactive(Filter(Negate(is.na), x()))
  module_return <- shiny::reactiveValues(code = TRUE, mask = TRUE)
  
  choices <- shiny::reactive(unique(as.character(sort(x_wo_NA()))))
  
  output$ui <- shiny::renderUI({
    filter_log("updating ui", verbose = verbose)
    shiny::div(style = "position: relative;",
               shiny::div(style = "
                          position: absolute; 
                          top: -2px; right: 16px; bottom: -2px; left: 16px;
                          animation: 
                          0.75s ease-out 0s 1 shinyDataFilterEnlargeX, 
                          0.5s ease-in  0s 1 shinyDataFilterFadeIn; 
                          transform-origin: left;" #,
               ),
               shiny::checkboxGroupInput(ns("param"), NULL,
                                         choices = choices(),
                                         selected = shiny::isolate(input$param) %||% c(),
                                         width = "100%"))
  })
  
  # Normalized
  # ggplot2::ggplot() + 
  #   # sort factor so that it reflects checkbox order
  #   ggplot2::aes(x = factor(
  #     as.character(x_wo_NA()), 
  #     levels = rev(choices()))) + 
  #   ggplot2::geom_bar(
  #     width = 0.95,
  #     fill = grDevices::rgb(66/255, 139/255, 202/255), 
  #     color = NA, 
  #     alpha = 0.2) +
  #   ggplot2::coord_flip() +
  #   ggplot2::theme_void() + 
  #   ggplot2::scale_x_discrete(expand = c(0, 0)) +
  #   ggplot2::scale_y_continuous(expand = c(0, 0))
  
  module_return$code <- shiny::reactive({
    if (length(input$param))
      bquote(.x %in% .(c(if (filter_na()) c() else NA, input$param)))
    else if (filter_na())
      bquote(!is.na(.x))
    else
      TRUE
  })
  
  module_return$mask <- shiny::reactive({
    eval(do.call(substitute, list(module_return$code(), list(.x = x())))) # added numeric() to return val, got errors. Then removed
  })
  
  module_return
}