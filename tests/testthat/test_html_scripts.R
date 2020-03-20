context("test assorted html scripts")

app_path <- shinyDataFilter:::shinytest_path("shinytest_html_scripts")
app <- shinytest::ShinyDriver$new(app_path)

test_that("shinyDataFilter css gets added to header", {
  expect_true(grepl('style id="shinyDataFilter-animations"', app$getSource()))
  expect_true(grepl('style id="shinyDataFilter-css"', app$getSource()))
  expect_true(grepl('style id="shinyDataFilter-SortableJS-css"', app$getSource()))
  expect_true(grepl('script id="shinyDataFilter-SortableJS-js"', app$getSource()))
})

app$stop()
