css_sortableJS_style_script <- function() {
  shiny::singleton(tags$head(tags$style(
    id = "shinyDataFilter-SortableJS-css", 
    type = "text/css", 
    gsub("\\s+", " ", "
      .sortableJS-handle {
        opacity: 0.1;
        cursor: move;
        cursor: -webkit-grabbing;
      }")
  )))
}

js_sortableJS_script <- function() {
  shiny::tagList(
    shiny::singleton(tags$head(tags$script(
      id = "shinyDataFilter-SortableJS-js",
      src = "shinyDataFilter_shared/Sortable.min.js"))),
    shiny::singleton(tags$head(tags$script(
      id = "shinyDataFilter-SortableJS-binding",
      gsub("\\s+", " ", "
        var sortableListBinding = new Shiny.InputBinding();
        $.extend(sortableListBinding, {
          find: function(scope) {
            return $(scope).find(\".listWithHandle\");
          },
          getValue: function(el) {
            if (typeof Sortable.active != 'undefined'){
              return Sortable.active.toArray();
            }
            else return \"\";
          },
          subscribe: function(el, callback) {
            $(el).on(\"end.sortableListBinding\", function(e) {
              callback();
            });
          },
          unsubscribe: function(el) {
            $(el).off(\".sortableListBinding\");
          },
          initialize: function(el) {
            Sortable.create(el, {
              handle: '.sortableJS-handle',
              animation: 150,
              onEnd: function (evt) {
                $(el).trigger(\"end\");
            }});
          }
        });
        
        Shiny.inputBindings.register(sortableListBinding);")
  ))))
}