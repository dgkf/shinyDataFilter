context("test_shiny_vector_filter_unknown")

app_path <- shinyDataFilter:::shinytest_path("shinytest_shiny_vector_filter")
app <- shinytest::ShinyDriver$new(app_path)

data <- list(1, 2, 3)
app$setInputs(`data_dput` = paste(capture.output(dput(data)), paste = "\n"))
app$waitFor("data_display")



test_that("testing that unknown datatypes show error message", {
  expect_true(grepl("don't know how to ", app$getValue("data_display")))
})



app$stop()
