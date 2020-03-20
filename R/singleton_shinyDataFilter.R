css_shinyDataFilter_animation_script <- function() {
  shiny::singleton(tags$head(tags$style(
    id = "shinyDataFilter-animations", 
    type = "text/css", 
    gsub("\\s+", " ", "
      @keyframes shinyDataFilterFadeIn {
        0% {
          opacity: 0;
        }
        100% {
          opacity: 1;
        }
      }
      
      @keyframes shinyDataFilterEnlargeY {
        0% {
          transform: scaleY(0.5);
        }
        100% {
          transform: scaleY(1);
        }
      }
      
      @keyframes shinyDataFilterEnlargeX {
        0% {
          transform: scaleX(0.5);
        }
        100% {
          transform: scaleX(1);
        }
      }")
  )))
}

css_shinyDataFilter_style_script <- function() {
  shiny::singleton(tags$head(tags$style(
    id = "shinyDataFilter-css", 
    type = "text/css", 
    gsub("\\s+", " ", "
      .shinyDataFilterStrikeout {
        position: relative;
      }
      
      .shinyDataFilterStrikeout::before {
        border-bottom: 0.25em solid;
        opacity: 0.5;
        content: \"\";
        left: 0;
        line-height: 1em;
        margin-top: calc(0.125em / 2 * -1);
        position: absolute;
        right: 0;
        top: 50%;
      }

      select[id$=\"add_filter_select\"] + div .selectize-input {
        text-align: center;
        padding: 6px 12px;
        border: 1px #e3e3e3 solid;
        border-radius: 4px;
        background-color: #f5f5f5;
      }
      
      select[id$=\"add_filter_select\"] + div .selectize-input:hover {
        background-color: #e6e6e6;
        border-color: #adadad;
      }

      select[id$=\"add_filter_select\"] + div .selectize-input:after {
        content: none !important;
      }
      ")
  )))
}
