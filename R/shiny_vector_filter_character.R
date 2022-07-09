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

    module_return <- shiny::reactiveValues(code = TRUE, mask = TRUE)

    output$ui <- shiny::renderUI({
      filter_log("updating ui", verbose = verbose)
      shiny::textInput(
        ns("param"),
        NULL,
        value = shiny::isolate(input$param),
        width = "100%"
      )
    })

    module_return$code <- shiny::reactive({
      if (is.null(input$param) || !nchar(input$param))
        TRUE
      else if (filter_na())
        bquote(grepl(.(tolower(input$param)), tolower(.x), fixed = TRUE))
      else
        bquote(is.na(.x) | grepl(.(tolower(input$param)), tolower(.x), fixed = TRUE))
    })

    module_return$mask <- shiny::reactive({
      eval(do.call(substitute, list(module_return$code(), list(.x = x()))))
    })

    module_return
  }
}
