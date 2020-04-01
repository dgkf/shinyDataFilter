library(shiny)
library(IDEAFilter)

library(dplyr)  # for data preprocessing and example data

# prep a new data.frame with more diverse data types
starwars2 <- starwars %>%
  mutate_if(~is.numeric(.) && all(Filter(Negate(is.na), .) %% 1 == 0), as.integer) %>%
  mutate_if(~is.character(.) && length(unique(.)) <= 25, as.factor) %>%
  mutate(is_droid = species == "Droid") %>%
  select(name, gender, height, mass, hair_color, eye_color, vehicles, is_droid)

ui <- fluidPage(
  titlePanel("Filter Data Example"),
  fluidRow(
    column(8,
      dataTableOutput("data_summary"),
      verbatimTextOutput("data_filter_code")),
    column(4, shiny_data_filter_ui("data_filter"))))

server <- function(input, output, session) {
  filtered_data <- callModule(
    shiny_data_filter,
    "data_filter",
    data = starwars2,
    verbose = FALSE)

  output$data_filter_code <- renderPrint({
    cat(gsub("%>%", "%>% \n ",
      gsub("\\s{2,}", " ",
        paste0(
          capture.output(attr(filtered_data(), "code")),
          collapse = " "))
    ))
  })

  output$data_summary <- renderDataTable({
    filtered_data() 
  }, 
  options = list(
    scrollX = TRUE,
    pageLength = 5
  ))
}

shinyApp(ui = ui, server = server)