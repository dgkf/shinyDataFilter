context("test_shiny_vector_filter_factor")

app_path <- shinyDataFilter:::shinytest_path("shinytest_shiny_vector_filter")
app <- shinytest::ShinyDriver$new(app_path)

data <- as.factor(c(letters[1:10], NA))
app$setInputs(`data_dput` = paste(capture.output(dput(data)), collapse = "\n"))
app$waitFor("test_in-param")



test_that("testing that factor (many) vectors get filtered properly", {
  app$setInputs(`test_in-param` = c("a", "b"))
  app$setInputs(`filter_na` = TRUE)
  
  expect_equal(
    app$getValue("test_mask"), 
    renderPrint(data %in% c("a", "b"))())
  
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



test_that("testing that factor (many) vector filter code builds properly", {
  app$setInputs(`test_in-param` = c("d", "c"))
  app$setInputs(`filter_na` = TRUE)
  
  expect_equal(
    app$getValue("test_code"), 
    renderPrint(quote(.x %in% c("d", "c")))())
  
  app$setInputs(`filter_na` = FALSE)
  
  expect_equal(
    app$getValue("test_code"), 
    renderPrint(quote(.x %in% c(NA, "d", "c")))())
})



data <- as.factor(c(letters[1:3], NA))
app$setInputs(`data_dput` = paste(capture.output(dput(data)), collapse = "\n"))



test_that("testing that factor (few) vectors get filtered properly", {
  app$setInputs(`test_in-param` = c("a", "b"))
  app$setInputs(`filter_na` = TRUE)
  
  expect_equal(
    app$getValue("test_mask"), 
    renderPrint(data %in% c("a", "b"))())
  
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



test_that("testing that factor (few) vector filter code builds properly", {
  app$setInputs(`test_in-param` = c("c", "a"))
  app$setInputs(`filter_na` = TRUE)
  
  expect_equal(
    app$getValue("test_code"), 
    renderPrint(quote(.x %in% c("a", "c")))())
  
  app$setInputs(`filter_na` = FALSE)
  
  expect_equal(
    app$getValue("test_code"), 
    renderPrint(quote(.x %in% c(NA, "a", "c")))())
})



app$stop()
