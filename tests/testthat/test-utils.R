test_that("infer_model_type returns the expected models", {
  testthat::expect_equal(infer_model_type(LETTERS), 3)
  testthat::expect_equal(infer_model_type(iris$Sepal.Length), 2)
  testthat::expect_equal(infer_model_type(iris$Species), 4)
  testthat::expect_equal(infer_model_type(sample(c(TRUE, FALSE), 10, replace = TRUE)), 1)
})

test_that("infer_model_type returns error when not supplied with a vector", {
  testthat::expect_error(infer_model_type(iris))
  testthat::expect_error(infer_model_type(as.list(LETTERS)))
})
