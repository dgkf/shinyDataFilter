shinyDataFilter_resourcePath <- function() {
  singleton(shiny::addResourcePath(
    "shinyDataFilter_shared",
    system.file(package = "IDEAFilter", "www", "shared")))
}