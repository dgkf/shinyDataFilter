
# shinyDataFilter

data-agnostic, shiny-idiomatic filter module

Built on top of [Joe Cheng](https://github.com/jcheng5)'s excellent [R/Pharma 2018 shiny demo](https://github.com/jcheng5/rpharma-demo) and experimenting with pushing his concept of hyper-modular shiny components as far as we could. In addition to what Joe showed off at the time, this shiny module comes with drag-and-drop reordering and overlayed visualizations of each filter variable's data qualities.

# Getting started

## Installation

``` r
# install.packages("devtools")
devtools::install_github("dgkf/shinyDataFilter")
```

## Example App

Then, run this sample app to build filters with `shinyDataFilter`:

``` r
library(shiny)
library(dplyr)

shinyAppFile(system.file("examples", "basic_app", "app.R", package = "shinyDataFilter"))
```
