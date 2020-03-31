ui <- fluidPage(
  shinyDataFilter_Maya:::css_shinyDataFilter_animation_script(),
  shinyDataFilter_Maya:::css_shinyDataFilter_style_script(),
  shinyDataFilter_Maya:::css_sortableJS_style_script(),
  shinyDataFilter_Maya:::js_sortableJS_script())

srv <- function(input, output, session) { }

shinyApp(ui, srv)
