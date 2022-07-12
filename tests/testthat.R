library(testthat)
library(shinytest)
library(shinyDataFilter)

if (!shinytest::dependenciesInstalled()) {
  warning("tests cannot be run because shinytest dependencies are not available")
} else {
  test_check("shinyDataFilter")
}
