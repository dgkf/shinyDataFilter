context("test_shiny_vector_filter_numeric")

app_path <- shinyDataFilter:::shinytest_path("shinytest_shiny_vector_filter")
app <- shinytest::ShinyDriver$new(app_path)

data <- c(1:9, NA)
app$setInputs(`data_dput` = paste(capture.output(dput(data)), paste = "\n"))
app$waitFor("test_in-param")


test_that("testing that numeric vectors get filtered properly", {
  app$setInputs(`test_in-param` = c(3, 6))
  app$setInputs(`filter_na` = TRUE)

  expect_equal(
    app$getValue("test_mask"),
    renderPrint(data >= 3 & data <= 6)())

  expect_true({
    filtered_data <- eval(parse(text = app$getValue("test_filtered_dput")))
    !any(is.na(filtered_data))
  })

  app$setInputs(`filter_na` = FALSE)

  expect_true({
    filtered_data <- eval(parse(text = app$getValue("test_filtered_dput")))
    any(is.na(filtered_data))
  })
})



test_that("testing that numeric vector filter code builds properly", {
  app$setInputs(`test_in-param` = c(5, 8))
  app$setInputs(`filter_na` = TRUE)

  expect_equal(
    app$getValue("test_code"),
    renderPrint(quote(.x >= 5 & .x <= 8))())

  app$setInputs(`filter_na` = FALSE)

  expect_equal(
    app$getValue("test_code"),
    renderPrint(quote(is.na(.x) | (.x >= 5 & .x <= 8)))())
})



test_that("testing that numeric vector filter builds a plot", {
  app$setInputs(`test_in-param` = c(5, 8))
  app$setInputs(`filter_na` = TRUE)

  expect_true({
    all(grep("data:image/png", app$getAllValues()$output$`test_in-plot`$src))
  })
})


app$finalize()
