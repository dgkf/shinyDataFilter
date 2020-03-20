
# shinyDataFilter <a href='https://github.com/dgkf/shinyDataFilter'><img src='assets/hex-shinyDataFilter.png' align="right" height="139" /></a>

data-agnostic, shiny-idiomatic filter module

![shinyDataFilter](https://user-images.githubusercontent.com/18220321/77127982-b6a9fe80-6a0b-11ea-8233-e77e0f362d70.gif)

Built on top of [Joe Cheng](https://github.com/jcheng5)’s excellent
[R/Pharma 2018 shiny demo](https://github.com/jcheng5/rpharma-demo) and
experimenting with pushing his concept of hyper-modular shiny components
as far as we could. In addition to what Joe showed off at the time, this
shiny module comes with drag-and-drop reordering and overlayed
visualizations of each filter variable’s data qualities.

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
shinyAppFile(system.file("examples", "basic_app", "app.R", package = "shinyDataFilter"))
```
