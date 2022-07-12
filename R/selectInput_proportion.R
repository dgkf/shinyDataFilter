#' A selectizeInput customized for unique element select from vector
#'
#' @param inputId passed to \code{\link[shiny]{selectizeInput}}
#' @param label passed to \code{\link[shiny]{selectizeInput}}
#' @param vec \code{vector} object from which unique terms should be sourced
#' @param selected default selection
#' @param ... passed to \code{\link[shiny]{selectizeInput}}
#' @param placeholder passed to \code{\link[shiny]{selectizeInput}} options
#' @param onInitialize passed to \code{\link[shiny]{selectizeInput}} options
#' @param sort how to sort fields in dropdown
#'
#' @return a \code{\link[shiny]{selectizeInput}} dropdown element
#'
#' @importFrom shiny selectizeInput
#'
proportionSelectInput <- function(inputId, label, vec, selected = "", ...,
    placeholder = "", onInitialize, sort = c("count", "alpha", "order")) {

  sort <- match.arg(sort, c("count", "alpha", "order"), several.ok = FALSE)

  vecr <- if (is.reactive(vec)) vec else reactive(vec)

  vecr_counts <- sort(table(vecr()), decreasing = TRUE)
  vecr_names <- names(vecr_counts)
  vecr_counts <- as.numeric(vecr_counts)
  names(vecr_counts) <- vecr_names
  vecr_props  <- vecr_counts / sum(vecr_counts)

  if (sort == "count") {
    vecr_unique <- names(vecr_counts)
  } else if (sort == "alpha") {
    vecr_unique <- as.character(sort(unique(Filter(Negate(is.na), vecr()))))
  } else {
    vecr_unique <- unique(Filter(Negate(is.na), vecr()))
  }

  labels <- Map(function(v) {
    json <- sprintf(strip_leading_ws('
    {
      "name": "%s",
      "prop": %f,
      "count": %d
    }'),
    v, vecr_props[[v]], vecr_counts[[v]])
  }, vecr_unique)

  choices <- as.list(vecr_unique)
  names(choices) <- labels

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
          return '<div style=\"position: relative;\">' +
                    '<div style=\"position: absolute; top: 5%; bottom: 5%; left: 0%; width: ' + item.data.prop * 100 + '%; background-color: #428BCA; opacity: 0.2;\"></div>' +
                    '<div style=\"z-index: 1;\">' +
                      escape(item.data.name) + ' ' +
                      '<strong style=\"opacity: 0.3;\">' + escape(item.data.count) + '</strong>' +
                    '</div>' +
                 '</div>';
        },

        // avoid data vomit splashing on screen when an option is selected
        item: function(item, escape) {
          item.data = JSON.parse(item.label);
          return '<div style=\"padding-left: 0.5em; padding-right: 0.5em;\">' +
                   escape(item.value) + ' ' +
                   '<strong style=\"opacity: 0.3;\">' +
                     '(' + escape(item.data.count) + ')' +
                   '</strong>' +
                 '</div>';
        }
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
