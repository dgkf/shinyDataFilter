#' @importFrom shiny uiOutput
#' @export
shiny_vector_filter_ui.numeric = function(data, inputId) {
  ns <- NS(inputId)
  shiny::uiOutput(ns("ui"))
}



#' @importFrom shiny reactive reactiveValues renderUI div plotOutput sliderInput
#'   isolate tags validate need renderPlot
#' @importFrom ggplot2 ggplot aes aes_ geom_area theme_void scale_x_continuous
#'   scale_y_continuous
#' @importFrom grDevices rgb
#' @importFrom stats density
#' @export
shiny_vector_filter.numeric <- function(data, inputId, ...) {
  function(input, output, session, x = shiny::reactive(numeric()),
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
          transform-origin: bottom;",
          shiny::plotOutput(ns("plot"), height = "100%")),
        if (any(!is.na(x()))) {
          shiny::sliderInput(ns("param"), NULL,
            value = shiny::isolate(input$param) %||% range(x(), na.rm = TRUE),
            min = min(x(), na.rm = TRUE),
            max = max(x(), na.rm = TRUE))
        } else {
          shiny::div(
            style = "padding-top: 10px; opacity: 0.3; text-align: center;",
            shiny::tags$h5(shiny::tags$i("no numeric values")))
        })
    })

    x_density_df <- shiny::reactive({
      shiny::validate(shiny::need(sum(!is.na(x())) > 1, ""))
      d <- stats::density(x(), na.rm = TRUE)
      data.frame(x = d$x, y = d$y)
    })

    output$plot <- shiny::renderPlot(
      bg = 'transparent',
      height = 25, {
      shiny::validate(shiny::need("data.frame" %in% class(x_density_df()), ""))
      ggplot2::ggplot(x_density_df()) +
        ggplot2::aes_(x = ~x, y = ~y) +
        ggplot2::geom_area(
          fill = grDevices::rgb(66/255, 139/255, 202/255),
          color = NA,
          alpha = 0.2) +
        ggplot2::theme_void() +
        ggplot2::scale_y_continuous(expand = c(0,0)) +
        ggplot2::scale_x_continuous(expand = c(0,0))
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
}
