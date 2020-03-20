shinyDataFilter_resourcePath <- function() {
  singleton(shiny::addResourcePath(
    "shinyDataFilter_shared",
    system.file(package = "shinyDataFilter", "www", "shared")))
}