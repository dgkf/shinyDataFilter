ui <- fluidPage(
  textInput("data_dput", "data dput"),
  checkboxInput("filter_na", "filter_na"),
  checkboxInput("verbose", "verbose"),
  uiOutput("data_display"),
  p("original data"),
  verbatimTextOutput("orig_data"),
  p("filtered data dput"),
  verbatimTextOutput("test_filtered_dput"),
  p("filtered data"),
  verbatimTextOutput("test_filtered"),
  p("data mask"),
  verbatimTextOutput("test_mask"),
  p("mask code"),
  verbatimTextOutput("test_code"))

srv <- function(input, output, session) {
  data <- reactive({
    tryCatch({
      eval(parse(text = input$data_dput))
    }, error = function(e) NULL)
  })
  
  filter_na <- reactive(input$filter_na)
  
  filter_srv <- eventReactive(data(), {
    message(isolate(data()))
    shinyDataFilter_Maya::shiny_vector_filter(isolate(data()), "test_in")
  })
  
  filter <- reactive(callModule(
    filter_srv(),
    "test_in",
    x = data,
    filter_na = filter_na,
    verbose = FALSE))
  
  output$data_display <- renderUI({
    data()
    shinyDataFilter_Maya::shiny_vector_filter_ui(isolate(data()), "test_in")
  })
  
  output$orig_data <- shiny::renderPrint(data())
  output$test_filtered_dput <- shiny::renderPrint(dput(subset(data(), filter()$mask())))
  output$test_filtered <- shiny::renderPrint(subset(data(), filter()$mask()))
  output$test_mask <- shiny::renderPrint(filter()$mask())
  output$test_code <- shiny::renderPrint(filter()$code())
}

shinyApp(ui, srv)
