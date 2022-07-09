#' User interface function to add a data filter panel
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @return a shiny \code{\link[shiny]{tagList}} containing the filter ui
#'
#' @import shiny
#'
#' @importFrom shiny NS tagList div actionButton icon
#' @export
#'
#' @seealso \link{shiny_data_filter}
#'
#' @inherit shiny_data_filter examples
#'
shiny_data_filter_ui <- function(inputId) {
  ns <- shiny::NS(inputId)

  shinyDataFilter_resourcePath()

  shiny::tagList(
    css_sortableJS_style_script(),
    js_sortableJS_script(),
    css_shinyDataFilter_animation_script(),
    css_shinyDataFilter_style_script(),
    shiny::div(
      id = ns("sortableList"),
      class = "listWithHandle list-group",
      style = "margin-bottom: 0;"
    ),
    shiny::div(
      id = "shinyDataFilter-addFilter",
      width = "100%",
      uiOutput(ns("add_filter_select_ui"))
    )
  )
}



#' Shiny data filter module server function
#'
#' @param input requisite shiny module field specifying incoming ui input
#'   reactiveValues
#' @param output requisite shiny module field capturing output for the shiny
#'   data filter ui
#' @param session requisite shiny module field containing the active shiny
#'   session
#' @param data a \code{data.frame} or \code{reactive expression} returning a
#'   \code{data.frame} to use as the input to the filter module
#' @inheritParams columnSelectInput
#' @param verbose a \code{logical} value indicating whether or not to print log
#'   statements out to the console
#'
#' @return a \code{reactive expression} which returns the filtered data wrapped
#'   in an additional class, "shinyDataFilter_df". This structuer also contains
#'   a "code" field which represents the code needed to generate the filtered
#'   data.
#'
#' @seealso \link{shiny_data_filter_ui}
#'
#' @import shiny
#' @importFrom utils head tail
#' @export
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(shinyDataFilter)
#'
#' library(dplyr)  # for data preprocessing and example data
#'
#' # prep a new data.frame with more diverse data types
#' starwars2 <- starwars %>%
#'   mutate_if(~is.numeric(.) && all(Filter(Negate(is.na), .) %% 1 == 0), as.integer) %>%
#'   mutate_if(~is.character(.) && length(unique(.)) <= 25, as.factor) %>%
#'   mutate(is_droid = species == "Droid") %>%
#'   select(name, gender, height, mass, hair_color, eye_color, vehicles, is_droid)
#'
#' # create some labels to showcase column select input
#' attr(starwars2$name, "label")     <- "name of character"
#' attr(starwars2$gender, "label")   <- "gender of character"
#' attr(starwars2$height, "label")   <- "height of character in centimeters"
#' attr(starwars2$mass, "label")     <- "mass of character in kilograms"
#' attr(starwars2$is_droid, "label") <- "whether character is a droid"
#'
#' ui <- fluidPage(
#'   titlePanel("Filter Data Example"),
#'   fluidRow(
#'     column(8,
#'       verbatimTextOutput("data_summary"),
#'       verbatimTextOutput("data_filter_code")
#'     ),
#'     column(4,
#'       shiny_data_filter_ui("data_filter")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   filtered_data <- callModule(
#'     shiny_data_filter,
#'     "data_filter",
#'     data = starwars2,
#'     verbose = TRUE
#'   )
#'
#'   output$data_filter_code <- renderPrint({
#'     cat(gsub("%>%", "%>% \n ",
#'       gsub("\\s{2,}", " ",
#'         paste0(
#'           capture.output(attr(filtered_data(), "code")),
#'           collapse = " "
#'         )
#'       )
#'     ))
#'   })
#'
#'   output$data_summary <- renderPrint({
#'     if (nrow(filtered_data())) show(filtered_data())
#'     else "No data available"
#'   })
#' }
#'
#' shinyApp(ui = ui, server = server)
#' }
#'
shiny_data_filter <- function(input, output, session, data,
  choices = names, verbose = FALSE) {

  ns <- session$ns
  filter_log("calling module", verbose = verbose)

  # retrieve input from callModule call (sys.call(-5L))
  data_call <- as.list(sys.call(-5L))$data
  datar <- if (is.reactive(data)) data else reactive(data)

  filter_counter <- 0
  next_filter_id <- function() {
    filter_counter <<- filter_counter + 1
    sprintf("filter_%d", filter_counter)
  }

  filters <- reactiveVal(c("filter_0"))
  filter_returns <- list(filter_0 = reactiveValues(
    data = datar,
    code = reactive(TRUE),
    remove = NULL
  ))

  update_filter <- function(fid, in_fid, column_name = NULL) {
    fs <- isolate(filters())

    if (missing(in_fid))
      if (fid %in% fs) in_fid <- fs[[utils::head(which(fid == fs), 1) - 1]]
      else in_fid <- utils::tail(fs, 1)

    if (!in_fid %in% fs | !in_fid %in% names(filter_returns))
      stop('no known filter for inbound filter id.')

    if (fid %in% names(filter_returns)) {
      column_name <- filter_returns[[fid]]$column_name
      filter_returns[[fid]]$destroy
    }

    filter_returns[[fid]] <<- callModule(
      shiny_data_filter_item,
      fid,
      data = filter_returns[[in_fid]]$data,
      choices = choices,
      column_name = column_name,
      verbose = verbose
    )
  }

  output$add_filter_select_ui <- renderUI({
    columnSelectInput(
      ns("add_filter_select"),
      label = NULL,
      data = datar,
      choices = choices,
      placeholder = "Add Filter",
      width = "100%")
  })

  observe({
    filter_log("scrubbing filters tagged for removal", verbose = verbose)
    for (fid in filters()[-1])
      if (isTRUE(filter_returns[[fid]]$remove)) {
        idx <- utils::head(which(filters() == fid), 1)
        filter_returns[[fid]]$destroy
        filters(setdiff(filters(), fid))

        # overwrite existing module call with one taking new input data
        if (idx <= length(filters())) {
          update_filter(filters()[[idx]])
        }

        removeUI(selector = sprintf("#%s-ui", ns(fid)))
        break
      }
  })

  observeEvent(input$add_filter_btn, {
    filter_log("observing add filter button press", verbose = verbose)
    update_filter(fid <- next_filter_id())
    filters(append(filters(), fid))

    insertUI(
      selector = sprintf("#%s", ns("sortableList")),
      where = "beforeEnd",
      ui = shiny_data_filter_item_ui(ns(fid), verbose = verbose)
    )
  })

  observeEvent(input$add_filter_select, {
    if (!input$add_filter_select %in% names(datar())) return()

    filter_log("observing add filter button press", verbose = verbose)
    update_filter(fid <- next_filter_id(), column_name = input$add_filter_select)
    filters(append(filters(), fid))

    insertUI(
      selector = sprintf("#%s", ns("sortableList")),
      where = "beforeEnd",
      ui = shiny_data_filter_item_ui(ns(fid), verbose = verbose)
    )

    updateSelectInput(session, "add_filter_select", selected = "")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # observe drag-and-drop and update data flow
  observeEvent(input$sortableList, {
    old_filters <- filters()

    filters(c(
      filters()[1],  # preserve input 'filter'
      gsub(sprintf("^%s", ns("")), "", Filter(nchar, input$sortableList))
    ))

    filter_log("updating sortableJS list: ",
      paste(filters(), collapse = ", "),
      verbose = verbose)

    # update filters downstream of change, isolate to prevent premature updates
    idxs <- which(cumsum(old_filters != filters()) > 0)

    isolate(for (idx in idxs) update_filter(filters()[[idx]]))
  })

  code <- reactive({
    filter_log("building code", verbose = verbose)
    filter_exprs <- Filter(
      Negate(isTRUE),
      Map(function(fi) filter_returns[[fi]]$code(), filters()))

    Reduce(
      function(l,r) bquote(.(l) %>% filter(.(r))),
      filter_exprs,
      init = data_call)
  })

  reactive({
    filter_log("recalculating filtered data", verbose = verbose)
    structure(
      d <- filter_returns[[utils::tail(filters(), 1)]]$data(),
      code = code(),
      class = c("shinyDataFilter_df", class(d)))
  })
}
