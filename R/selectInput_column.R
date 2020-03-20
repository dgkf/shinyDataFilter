#' A selectizeInput customized for data frames with column labels
#'
#' @param inputId passed to \code{\link[shiny]{selectizeInput}}
#' @param label passed to \code{\link[shiny]{selectizeInput}}
#' @param data \code{data.frame} object from which fields should be populated
#' @param selected default selection
#' @param ... passed to \code{\link[shiny]{selectizeInput}}
#' @param placeholder passed to \code{\link[shiny]{selectizeInput}} options
#' @param onInitialize passed to \code{\link[shiny]{selectizeInput}} options 
#'
#' @return a \code{\link[shiny]{selectizeInput}} dropdown element
#' 
#' @importFrom shiny selectizeInput
#' 
columnSelectInput <- function(inputId, label, data, selected = "", ..., 
    placeholder = "", onInitialize) {
  
  datar <- if (is.reactive(data)) data else reactive(data)
  
  labels <- Map(function(col) {
    json <- sprintf(strip_leading_ws('
    {
      "name": "%s",
      "label": "%s",
      "datatype": "%s"
    }'), 
    col, 
    attr(datar()[[col]], "label") %||% "", 
    get_dataFilter_class(datar()[[col]]))
  }, col = names(datar()))
  choices <- setNames(names(datar()), labels)
  
  shiny::selectizeInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    ...,
    options = c(
      list(render = I("{
        // format the way that options are rendered
        option: function(item, escape) {
          item.data = JSON.parse(item.label);
          return '<div>' +
                   '<div><strong>' +
                      escape(item.data.name) + ' ' +
                      '<span style=\"opacity: 0.3;\"><code style=\"color: black;\"> ' + 
                        item.data.datatype + 
                      '</code></span>' + 
                   '</strong></div>' +
                   (item.data.label != '' ? '<div style=\"line-height: 1em;\"><small>' + escape(item.data.label) + '</small></div>' : '') + 
                 '</div>';
        },

        // avoid data vomit splashing on screen when an option is selected
        item: function(item, escape) { return ''; }
      }")),
      
      # fix for highlight persisting
      # https://github.com/selectize/selectize.js/issues/1141
      list(onType = I("function(str) { 
        str || this.$dropdown_content.removeHighlight();
      }")),
      
      list(onChange = I("function() { 
        this.$dropdown_content.removeHighlight();
      }")),
      
      # remove highlighting when losing focus
      list(onDropdownOpen = I("function(dropdown) {
        dropdown.removeHighlight();
      }")),
      
      # sort entries
      list(sortField = I("'value'")),
      
      # placeholder
      if (missing(placeholder)) list() 
      else list(placeholder = placeholder),
      
      # onInitialize
      if (missing(onInitialize) && !missing(placeholder))
        list(onInitialize = I('function() { this.setValue(""); }'))
      else if (!missing(onInitialize)) 
        list(onInitialize = onInitialize)
      else
        list()
    )
  )
}
