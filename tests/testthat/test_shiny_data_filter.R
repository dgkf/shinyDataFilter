context("test_shiny_data_filter")

app_path <- shinyDataFilter:::shinytest_path("shinytest_shiny_data_filter")
app <- shinytest::ShinyDriver$new(app_path)


app$setInputs(`data_filter-add_filter_select` = "Wind")
app$waitFor("data_filter-filter_1-remove_filter_btn")
app$setInputs(`data_filter-filter_1-remove_filter_btn` = "click")

test_that("test that a new filter item has been added", {
  expect_true(!"data_filter-filter_1-column_select" %in% app$listWidgets()$input)
})



app$setInputs(`data_filter-add_filter_select` = "Ozone")
app$waitFor("data_filter-filter_2-vector_filter-param")
app$setInputs(`data_filter-filter_2-vector_filter-param` = c(30, 90))

test_that("test that a new filter item has been added", {
  expect_equal(
    app$getValue("data_summary"),
    renderPrint(subset(airquality, is.na(Ozone) | (Ozone >= 30 & Ozone <= 90)))())
})



app$setInputs(`data_filter-filter_2-filter_na_btn` = "click")

test_that("test that a new filter item has been added", {
  expect_equal(
    app$getValue("data_summary"),
    renderPrint(subset(airquality, Ozone >= 30 & Ozone <= 90))())
})




app$setInputs(`data_filter-add_filter_select` = "Wind")
app$waitFor("data_filter-filter_3-vector_filter-param")
app$setInputs(`data_filter-filter_3-vector_filter-param` = c(5, 10))

test_that("test that nrow reactive value is accurate", {
  expect_equal(
    app$getValue("data_summary"),
    renderPrint(subset(airquality, 
      (Ozone >= 30 & Ozone <= 90) &
      (is.na(Wind) | (Wind >= 5 & Wind <= 10))
    ))())
})

app$stop()
