ui <- fluidPage(
  shinyDataFilter:::css_shinyDataFilter_animation_script(),
  shinyDataFilter:::css_shinyDataFilter_style_script(),
  shinyDataFilter:::css_sortableJS_style_script(),
  shinyDataFilter:::js_sortableJS_script())

srv <- function(input, output, session) { }

shinyApp(ui, srv)
