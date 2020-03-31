
# IDEAFilter

Built upon [shinyDataFilter](https://github.com/dgkf/shinyDataFilter),
which was built on on top of [Joe Cheng](https://github.com/jcheng5)’s
excellent [R/Pharma 2018 shiny
demo](https://github.com/jcheng5/rpharma-demo).

# Getting started

## Installation

``` r
# install.packages("devtools")
devtools::install_github("MayaGans/IDEAFilter")
```

## Example App

Then, run this sample app to build filters with `IDEAFilter`:

``` r
library(shiny)
shinyAppFile(system.file("examples", "basic_app", "app.R", package = "IDEAFilter"))
```
