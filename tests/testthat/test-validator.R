##### --- tests for parameter checks --- #####

test_that("check_missing_columns works", {
  s <- create_schema()
  s$feature_column_names <-
    c(colnames(iris)[1:4], "Stamen.Count", "Anther.Length")
  s$prediction_id_column_name <- "Species"
  expect_error(check_missing_columns(.data_frame = iris, .schema = s))
})

test_that("check_invalid_model_id works", {
  expect_error(check_invalid_model_id(1:10))
  expect_error(check_invalid_model_id(iris))
  expect_silent(check_invalid_model_id("A"))
})

test_that("check_invalid_model_version works", {
  expect_silent(check_invalid_model_version(1, 1))
  expect_error(check_invalid_model_version(1, 2))
  expect_silent(check_invalid_model_version("test", "prod"))
  expect_silent(check_invalid_model_version("test", 2))
  expect_silent(check_invalid_model_version("test", 3))
})

test_that("check_invalid_batch_id works", {
  expect_silent(check_invalid_batch_id(1, 1))
  expect_silent(check_invalid_batch_id("test", "prod"))
  expect_silent(check_invalid_batch_id("test", 2))
  expect_error(check_invalid_batch_id(1, 2))
})

test_that("check_invalid_model_type works", {
  expect_silent(check_invalid_model_type(1))
  expect_error(check_invalid_model_type("NUMERIC"))
  expect_error(check_invalid_model_type("test"))
})

test_that("check_invalid_environment works", {
  expect_silent(check_invalid_environment(1))
  expect_error(check_invalid_environment("NUMERIC"))
  expect_error(check_invalid_environment("PRODUCTION"))
})

test_that("check_existence_pred_act_shap works", {
  s <- create_schema()
  s$feature_column_names <- colnames(iris)[1:4]
  s$prediction_id_column_name <- "Species"
  expect_error(check_existence_pred_act_shap(s))

  s$prediction_label_column_name <- "Species"
  expect_silent(check_existence_pred_act_shap(s))
})

test_that("check_existence_preprod_pred_act works", {
  s <- create_schema()
  s$feature_column_names <- colnames(iris)[1:4]
  s$prediction_id_column_name <- "Species"
  expect_silent(check_existence_preprod_pred_act(s, 1))
  expect_error(check_existence_preprod_pred_act(s, 2))
  expect_error(check_existence_preprod_pred_act(s, 3))

  # better when we add the required fields
  s$prediction_label_column_name <- "Species"
  s$actual_label_column_name <- "Iris"
  expect_silent(check_existence_preprod_pred_act(s, 1))
  expect_silent(check_existence_preprod_pred_act(s, 2))
  expect_silent(check_existence_preprod_pred_act(s, 3))
})

##### --- end tests for parameter checks --- #####

##### --- tests for type checks --- #####

test_that("check_type_prediction_id works", {
  # `iris$Species` is `factor`, so expecting error
  s <- create_schema(prediction_id_column_name = "Species")
  col_types <- setNames(lapply(iris, class), names(iris))
  expect_error(check_type_prediction_id(s, col_types))

  # making it a string, works
  col_types$Species <- "string"
  expect_silent(check_type_prediction_id(s, col_types))
})

test_that("check_type_timestamp works", {
  s <- create_schema(timestamp_column_name = "Month")
  col_types <- setNames(lapply(airquality, class), names(airquality))
  expect_error(check_type_timestamp(s, col_types))

  col_types$Month <- "double"
  expect_silent(check_type_timestamp(s, col_types))
})


test_that("check_type_features works", {
  s <- create_schema(feature_column_names = names(iris)[1:4])
  col_types <- setNames(lapply(iris, class), names(iris))
  expect_error(check_type_features(s, col_types))

  col_types2 <- c(rep("double", 4), "string")
  names(col_types2) <- names(iris)
  expect_silent(check_type_features(s, col_types2))
})

test_that("check_type_pred_act_labels works", {
  # categorical or score-categorical models
  iris2 <- iris
  iris2$Species <- as.character(iris2$Species)
  iris2$Label <- iris2$Species
  s <- create_schema(feature_column_names = names(iris)[1:4],
                     prediction_label_column_name = "Species",
                     actual_label_column_name = "Label")
  ct <- setNames(lapply(iris2, class), names(iris2))
  ct$Species <- "string"
  ct$Label <- "string"

  expect_error(check_type_pred_act_labels(.model_type = 1, .schema = s, .column_types = ct))
  expect_error(check_type_pred_act_labels(.model_type = 2, .schema = s, .column_types = ct))
  expect_silent(check_type_pred_act_labels(.model_type = 3, .schema = s, .column_types = ct))
  expect_silent(check_type_pred_act_labels(.model_type = 4, .schema = s, .column_types = ct))
})

test_that("check_type_pred_act_scores works", {
  # categorical or score-categorical models
  iris2 <- iris
  iris2$Sepal.Length.Score <- sample(100, nrow(iris2), replace = TRUE)
  s <- create_schema(feature_column_names = names(iris)[1:4],
                     prediction_score_column_name = "Sepal.Length",
                     actual_score_column_name = "Sepal.Length.Score")
  ct <- setNames(lapply(iris2, class), names(iris2))
  ct$Sepal.Length <- "double"
  ct$Sepal.Length.Score <- "double"
  # only relevant for SCORE_CATEGORICAL models
  expect_silent(check_type_pred_act_scores(.model_type = 1, .schema = s, .column_types = ct))
  expect_silent(check_type_pred_act_scores(.model_type = 2, .schema = s, .column_types = ct))
  expect_silent(check_type_pred_act_scores(.model_type = 3, .schema = s, .column_types = ct))
  expect_silent(check_type_pred_act_scores(.model_type = 4, .schema = s, .column_types = ct))
  ct$Sepal.Length.Score <- "factor"
  expect_error(check_type_pred_act_scores(.model_type = 4, .schema = s, .column_types = ct))
  ct$Sepal.Length <- "string"
  expect_error(check_type_pred_act_scores(.model_type = 4, .schema = s, .column_types = ct))
})

##### --- end tests for type checks --- #####

test_that("check_value_timestamp works", {
  iris2 <- iris
  iris2$Timestamp_posixct <- Sys.time()
  iris2$Timestamp_date <- Sys.Date()
  iris2$Timestamp_int <- as.integer(Sys.time())
  iris2$Timestamp_num <- as.numeric(Sys.time())

  # datetime
  iris_schema <- create_schema(
    feature_column_names = names(iris2)[1:4],
    prediction_id_column_name = names(iris2)[5],
    timestamp_column_name = "Timestamp_posixct"
  )

  expect_silent(check_value_timestamp(.data_frame = iris2, .schema = iris_schema))

  # date
  iris_schema$timestamp_column_name <- "Timestamp_date"
  expect_silent(check_value_timestamp(.data_frame = iris2, .schema = iris_schema))

  # integer
  iris_schema$timestamp_column_name <- "Timestamp_int"
  expect_silent(check_value_timestamp(.data_frame = iris2, .schema = iris_schema))

  # numeric
  iris_schema$timestamp_column_name <- "Timestamp_num"
  expect_silent(check_value_timestamp(.data_frame = iris2, .schema = iris_schema))
})

test_that("check_value_missing works", {
  iris2 <- iris
  iris2$Species2 <- as.numeric(iris$Species)
  iris2$Iris <- iris2$Species2
  iris2$Observed <- iris2$Species2

  # datetime
  iris_schema <- create_schema(
    feature_column_names = names(iris2)[1:4],
    prediction_id_column_name = "Species2",
    prediction_label_column_name = "Iris",
    actual_label_column_name = "Observed"
  )

  check_value_missing(.data_frame = iris2, .schema = iris_schema)

  # add some missing data
  iris2$Iris[[5]] <- NA_real_
  expect_error(check_value_missing(.data_frame = iris2, .schema = iris_schema))

})
