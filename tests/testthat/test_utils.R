context("test_null_or")

test_that("testing that null or operator works", {
  `%||%` <- shinyDataFilter:::`%||%`
  expect_true(NULL %||% TRUE)
  expect_true(TRUE %||% NULL)
})
