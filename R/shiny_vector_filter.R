#' A stub UI for a vector filter module
#'
#' This is a stub of a UI which assumes any dispatched module will create the
#' needed UI in the "ui" uiOutput target
#'
#' @param data the data object to be filtered
#' @param inputId The \code{input} slot that will be used to access the value.
#'
#' @return an empty \code{link[shiny]{div}} or a \code{link[shiny]{uiOutput}}
#'   placeholder expecting output "ui"
#'
#' @importFrom shiny NS div uiOutput
#' @export
#'
shiny_vector_filter_ui <- function(data, inputId) {
  UseMethod("shiny_vector_filter_ui")
}



#' @importFrom shiny span code
#' @export
shiny_vector_filter_ui.default <- function(data, inputId) {
  shiny::div(style = "opacity: 0.5;",
    p(width = "100%",
      align = "center",
      "don't know how to filter class ",
      shiny::code(class(data))))
}



#' Vector-specific filter module server function
#'
#' The \code{shiny_vector_filter} family of S3 dispatched functions use the
#' input data to route to the appropriate ui and server functions for the vector
#' datatype.
#'
#' The \code{shiny_vector_filter} functions return the shiny module server
#' functions to use for the specified filter. These functions should follow the
#' following template:
#'
#' \preformatted{
#'   function(input, output, session, x, filter_na, verbose = FALSE) {
#'     # ... additional code here
#'     reactiveValues(code = TRUE, mask = TRUE)
#'   }
#' }
#'
#' This function will conform to the following specification
#'
#' \strong{Arguments}
#' \describe{
#' \item{\code{input}}{
#'   requisite shiny module field specifying incoming ui input reactiveValues
#' }
#' \item{\code{output}}{
#'   requisite shiny module field capturing output for the shiny data filter
#'   ui
#' }
#' \item{\code{session}}{
#'   requisite shiny module field containing the active shiny session
#' }
#' \item{\code{x}}{
#'   a reactive expression resolving to the vector to filter
#' }
#' \item{\code{filter_na}}{
#'   a logical value indicating whether to filter \code{NA} values from the
#'   \code{x} vector
#' }
#' \item{\code{verbose}}{
#'   a \code{logical} value indicating whether or not to print log
#'   statements out to the console
#' }
#' }
#'
#' \strong{Value}
#' a \code{\link[shiny]{reactiveValues}} list containing a logical vector
#' called "mask" which can be used to filter the provided vector and an element
#' "code" which is the expression used to generate the mask.
#'
#' @param data the data object to be filtered
#' @param inputId The \code{input} slot that will be used to access the value.
#'
#' @return a shiny server function as described in the details
#'
#' @export
#'
shiny_vector_filter <- function(data, inputId, global = FALSE) {
  if (!global && exists("shiny_vector_filter", envir = .GlobalEnv)) {
    args = list(data = data, inputId = inputId, global = TRUE)
    do.call("shiny_vector_filter", args, envir = .GlobalEnv)
  } else {
    UseMethod("shiny_vector_filter")
  }
}



#' @importFrom shiny reactive reactiveValues
#' @export
shiny_vector_filter.default <- function(data, inputId, ...) {
  function(input, output, session, x = shiny::reactive(NULL),
      filter_na = shiny::reactive(FALSE), verbose = FALSE) {

    module_return <- shiny::reactiveValues(code = FALSE, mask = FALSE)
    module_return$code <- shiny::reactive(FALSE)
    module_return$mask <- shiny::reactive(FALSE)

    module_return
  }
}



#' Return the class used for dispatch to shiny_vector_filter formatted using
#' pillar
#'
#' @param obj the object whose class for dispatch should be derived
#'
#' @return a pillar formatted class name
#'
#' @importFrom pillar new_pillar_type
#'
get_dataFilter_class <- function(obj) {
  vf_methods <- gsub(".*\\.", "", as.character(methods(shiny_vector_filter)))

  if ("numeric" %in% vf_methods)
    vf_methods <- c(vf_methods, "real", "double", "integer")

  vf_class <- class(obj)
  vf_class <- vf_class[vf_class %in% vf_methods]

  if (!length(vf_class)) return("unk")
  class(obj) <- vf_class
  pillar::new_pillar_type(obj)[[1]][1]
}
