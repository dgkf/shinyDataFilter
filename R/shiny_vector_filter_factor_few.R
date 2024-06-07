#' A vector filter for factors with only a few choices
#'
#' @param input requisite shiny module field specifying incoming ui input
#'   reactiveValues
#' @param output requisite shiny module field capturing output for the shiny
#'   data filter ui
#' @param session requisite shiny module field containing the active shiny
#'   session
#' @param x a reactive expression resolving to the vector to filter
#' @param filter_na a logical value indicating whether to filter \code{NA}
#'   values from the \code{x} vector
#' @param verbose a \code{logical} value indicating whether or not to print log
#'   statements out to the console
#'
#' @return a \code{\link[shiny]{reactiveValues}} list containing a logical
#'   vector called "mask" which can be used to filter the provided vector and an
#'   element "code" which is the expression used to generate the mask.
#'
#' @importFrom shiny reactive reactiveValues renderUI div plotOutput
#'   checkboxGroupInput renderPlot isolate
#' @importFrom ggplot2 ggplot aes aes_ geom_bar coord_flip theme_void
#'   scale_x_discrete scale_y_continuous
#' @importFrom grDevices rgb
shiny_vector_filter_factor_few <- function(input, output, session,
    x = shiny::reactive(factor()), filter_na = shiny::reactive(TRUE),
    verbose = FALSE) {

  ns <- session$ns

  x_wo_NA <- shiny::reactive(Filter(Negate(is.na), x()))
  module_return <- shiny::reactiveValues(code = TRUE, mask = TRUE)

  choices <- shiny::reactive(unique(as.character(x_wo_NA())))

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

  output$plot <- shiny::renderPlot(bg = 'transparent', {
    # Proportional
    ggplot2::ggplot() +
      # sort factor so that it reflects checkbox order
      ggplot2::aes(x = factor(
        as.character(x_wo_NA()),
        levels = rev(choices()))) +
      ggplot2::geom_bar(ggplot2::aes_(y = ~..count.. / sum(..count..)),
        width = 0.95,
        fill = grDevices::rgb(66/255, 139/255, 202/255),
        color = NA,
        alpha = 0.2) +
      ggplot2::coord_flip() +
      ggplot2::theme_void() +
      ggplot2::scale_x_discrete(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1))

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
    #eval(do.call(substitute, list(module_return$code(), list(.x = x()))))
    TRUE
  })

  module_return
}
