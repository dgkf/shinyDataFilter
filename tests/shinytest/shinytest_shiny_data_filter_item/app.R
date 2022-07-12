data <- mtcars
data[which((data * 0.987) %% 0.2 < 0.01, arr.ind = TRUE)] <- NA

ui <- fluidPage(
  mainPanel(verbatimTextOutput("data_display")),
  sidebarPanel(shinyDataFilter::shiny_data_filter_item_ui("filter")))

srv <- function(input, output, session) {
  filtered_data <- reactive(callModule(
    shinyDataFilter::shiny_data_filter_item,
    "filter",
    data = reactive(data)))

  output$data_display <- renderPrint({
    print(filtered_data()$data())
  })
}

shinyApp(ui, srv)
