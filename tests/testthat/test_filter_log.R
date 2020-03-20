context("test_filter_log")

test_that("testing that filter log produces messages", {
  expect_message(shinyDataFilter:::filter_log("test"))
  expect_message(shinyDataFilter:::filter_log("test"), "[filters]")
})

test_that("testing that filter log captures namespace", {
  expect_message(
    shinyDataFilter:::filter_log("test", ns = function(i) paste0("testing-", i)), 
    "testing")
})

test_that("testing that filter log captures multiline object statements", {
  expect_message(shinyDataFilter:::filter_log(list(1, 2, 3)))
})

test_that("testing that filter log prints with ns = NULL", {
  expect_message(shinyDataFilter:::filter_log("test", ns = NULL))
})
