#' Shorthand operator for NULL fallback
#' 
#' @name nullor
#' 
#' @param lhs lhs for null-or operation
#' @param rhs rhs for null-or operation
#' 
#' @examples 
#' `%||%` <- shinyDataFilter:::`%||%`
#' 
#' NULL %||% 1
#' # [1] 1
#' 
#' 1 %||% 2
#' # [1] 1
#' 
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs



#' A logging function that captures the shiny namespace
#' 
#' @param ... fields to be logged
#' @param ns the name of the current active namespace. if missing, will be
#'   derived from parent environment.
#' @param verbose whether log should be written
#' 
#' @return \code{NULL}
#'
#' @importFrom crayon black
#' @importFrom utils capture.output
#' 
filter_log <- function(..., ns, verbose = TRUE) isolate({
  if (!verbose) return()
  
  if (missing(ns))
    ns <- tryCatch({
      eval(quote(session$ns), envir = parent.frame())
    }, error = function(e) { 
      function(id) "" 
    })
  
  if (is.null(ns)) 
    ns <- function(id) ""
  
  dots <- Map(function(i) {
    if (!is.character(i)) 
      if (length(c <- utils::capture.output(i)) > 1) 
        paste0("\n", paste0(c, collapse = "\n"))
      else 
        c
    else i
  }, list(...))
  
 do.call("message", as.list(c(
    filter_log_style("[filters] "), 
    filter_log_ns_style(ns(NULL)), 
    do.call(crayon::black, c(dots, sep = "")))))
})



#' A crayon style for the filter log
#' 
#' @param ... passed to a \link{crayon} crayon object
#' 
#' @importFrom crayon make_style
#' @importFrom grDevices rgb
#' @importFrom utils tail
filter_log_style <- crayon::make_style(grDevices::rgb(0, 0, 0.7))



#' A crayon style derived from a rough hash of the namespace name
#' 
#' @param txt ns name to format
#' 
#' @importFrom crayon make_style black
#' @importFrom RColorBrewer brewer.pal
filter_log_ns_style <- function(txt) {
  if (nchar(txt)) {
    out <- c()
    for (t in substring(txt, 1, unlist(gregexpr('-|$', txt))-1)) {
      col <- sum(utf8ToInt(t)) %% 11 + 1
      sty <- crayon::make_style(RColorBrewer::brewer.pal(12, "Paired")[-11][[col]])
      out <- c(out, sty(utils::tail(strsplit(t, '-')[[1]], 1)))
    }
    paste(paste0(out, collapse = crayon::black("-")), "")
  } else {
    txt
  }
}



#' Helper to debug shinytests so they work interactively as well as during test
#'
#' @param path a path within the tests/shinytest/ directory
#'
#' @return a path that works irrespective of how the code is executed
#' 
#' @importFrom utils capture.output
#' 
shinytest_path <- function(path) {
  # catches
  #   * devtools::test()
  #   * testthat::test_dir(testthat::test_path())
  #   * testthat::auto_test_package()
  #   * covr::package_coverage()
  
  top_level_call_f <- utils::capture.output(as.list(sys.calls()[[1]])[[1]])
  
  if (any(grepl("shinyDataFilter", top_level_call_f)) ||
      !any(grepl("test", top_level_call_f))) {
    file.path(".", "tests", "shinytest", path)
  } else {
    file.path("..", "shinytest", path)
  }
}



#' Strip leading white space from a block of text
#'
#' @param txt text to strip leading whitespace from
#' @param simplify whether to simplify down to a character vector
#'
#' @return the block of text with entire columns of leading whitespace removed
#' 
strip_leading_ws <- function(txt, simplify = TRUE) {
  
  txt <- strsplit(txt, '\n')
  nws <- lapply(txt, function(t) min(nchar(gsub('(^\\s*).*', '\\1', t)), Inf))
  txt <- Map(function(txt, nws) {
    paste(if (is.finite(nws)) substring(txt, nws +1) else txt, collapse = '\n')
  }, txt, nws)
  
  if (!isFALSE(simplify) && length(txt)) {
    simplify2array(txt, higher = (simplify == "array"))
  } else {
    txt
  }
}
