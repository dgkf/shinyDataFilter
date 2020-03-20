ui <- fluidPage(
  titlePanel("Filter Data Example"),
  fluidRow(
    column(8,
      verbatimTextOutput("data_summary"),
      verbatimTextOutput("data_filter_code")),
    column(4, shinyDataFilter::shiny_data_filter_ui("data_filter"))))

srv <- function(input, output, session) {
  filtered_data <- callModule(
    shinyDataFilter::shiny_data_filter,
    "data_filter",
    data = airquality,
    verbose = FALSE)

  output$data_filter_code <- renderPrint({
    cat(gsub("%>%", "%>% \n ",
      gsub("\\s{2,}", " ",
        paste0(
          capture.output(attr(filtered_data(), "code")),
          collapse = " "))
    ))
  })

  output$data_summary <- renderPrint({
    if (nrow(filtered_data())) show(filtered_data())
    else "No data available"
  })
}

shinyApp(ui, srv)
