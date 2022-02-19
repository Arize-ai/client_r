##### --- parameter checks --- #####

#' check_missing_columns
#' @description Check if any of the columns declared in the schema are missing from the data frame
#' @param .data_frame the data
#' @param .schema the schema (see `?arize::create_schema`)
#' @importFrom checkmate check_choice
#' @importFrom glue glue glue_collapse
#' @examples
#' # all good
#' s <- create_schema(feature_column_names = colnames(iris)[1:4],
#'                    prediction_id_column_name = "Species")
#' check_missing_columns(.data_frame = iris, .schema = s)
#'
#' # column from schema not found in data frame
#' s <- create_schema(
#'   feature_column_names = c(colnames(iris)[1:4], "Stamen.Count", "Anther.Length"),
#'   prediction_id_column_name = "Species"
#' )
#' \dontrun{
#' check_missing_columns(.data_frame = iris, .schema = s)
#' }
#' @noRd
check_missing_columns <- function(.data_frame, .schema) {
  nonempty_fields_contents <- .schema %>% unlist() %>% unname()

  is_missing_column <-
    sapply(nonempty_fields_contents, function(colname) {
      !test_choice(colname, colnames(.data_frame))
    })

  if (any(is_missing_column)) {
    missing_columns <-
      names(is_missing_column)[sapply(is_missing_column, isTRUE)]
    stop(
      glue(
        'Columns {glue_collapse(missing_columns, sep = ", ", last = " and ")} are declared in the schema but are not found in the data frame.'
      )
    )
    invisible(missing_columns)
  }
}

#' check_invalid_model_id
#' @description Check if the model id is valid
#' @param .model_id character, the id of the model
#' @importFrom checkmate assert_character
#' @examples
#' \dontrun{
#' # some of these fail
#' check_invalid_model_id("A")
#' check_invalid_model_id(1:10)
#' check_invalid_model_id(iris)
#' }
#' @noRd
check_invalid_model_id <- function(.model_id) {
  assert_character(x = .model_id, min.chars = 1)
}

#' check_invalid_model_version
#' @description Check if the model version is valid
#' @param .model_version character, the model version
#' @param .environment integer, `1` for production, `2` for validation, `3` for training
#' @importFrom checkmate assert_choice assert_character
#' @examples
#' \dontrun{
#' # some of these fail
#' check_invalid_model_version(1, 1)
#' check_invalid_model_version("test", 3)
#' check_invalid_model_version("test", "prod")
#' }
#' @noRd
check_invalid_model_version <- function(.model_version, .environment) {
  if (test_choice(.environment, 2:3)) {
    assert_character(.model_version, min.chars = 1)
  }
}

#' check_invalid_batch_id
#' @description Check if the batch id is valid
#' @param .batch_id character, the batch id
#' @param .environment integer, `1` for production, `2` for validation, `3` for training
#' @importFrom checkmate test_choice assert_character
#' @examples
#' \dontrun{
#' # some of these fail
#' check_invalid_batch_id(1, 1)
#' check_invalid_batch_id("test", 3)
#' check_invalid_batch_id("test", "prod")
#' }
#' @noRd
check_invalid_batch_id <- function(.batch_id, .environment) {
  if (test_choice(.environment, 2))
    assert_character(.batch_id, min.chars = 1)
}

#' check_invalid_model_type
#' @description Check if the model type is valid
#' @param .model_type The type of the model, see the `arize::model_types` data object
#' @importFrom checkmate test_choice
#' @examples
#' \dontrun{
#' # some of these fail
#' check_invalid_model_type(1)
#' check_invalid_model_type("NUMERIC")
#' check_invalid_model_type("test")
#' }
#' @noRd
check_invalid_model_type <- function(.model_type) {
  is_valid <- test_choice(x = .model_type, unlist(model_types))
  if (!is_valid) {
    stop(
      glue('Model type not valid. Choose one of the following:
           {glue_collapse(names(model_types), sep=", ", last = " and ")}.
           See https://docs.arize.com/arize/concepts-and-terminology/model-types')
    )
  }
}

#' check_invalid_environment
#' @description Check if the model type is valid
#' @param .environment integer, `1` for production, `2` for validation, `3` for training
#' @importFrom checkmate test_choice
#' @examples
#' \dontrun{
#' # some of these fail
#' check_invalid_environment(1)
#' check_invalid_environment("NUMERIC")
#' check_invalid_environment("PRODUCTION")
#' }
#' @noRd
check_invalid_environment <- function(.environment) {
  is_valid <- test_choice(x = .environment, unlist(environments))
  if (!is_valid) {
    stop(
      glue('Environment not valid. Choose one of the following:
           {glue_collapse(names(environments), sep=", ", last = " and ")}.
           See https://docs.arize.com/arize/concepts-and-terminology/model-environments')
    )
  }
}

#' check_existence_pred_act_shap
#' @description Check if the schema contains required fields
#' @param .schema the schema (see `?arize::create_schema`
#' @examples
#' # schema missing prediction label, actual label or SHAP column names
#' s <- create_schema(feature_column_names = colnames(iris)[1:4],
#'                    prediction_id_column_name = "Species")
#' \dontrun{
#' check_existence_pred_act_shap(s)
#' }
#'
#' # better when we add one of these
#' s$prediction_label_column_name <- "Species"
#' check_existence_pred_act_shap(s)
#' @noRd
check_existence_pred_act_shap <- function(.schema) {
  is_null_pred <- is.null(.schema$prediction_label_column_name)
  is_null_act  <- is.null(.schema$actual_label_column_name)
  is_null_shap <- is.null(.schema$shap_values_column_names)
  if (all(c(is_null_pred, is_null_act, is_null_shap))) {
    stop(
      "The schema must specify at least one of the following: prediction label, actual label, or SHAP value column names"
    )
  }
}

#' check_existence_preprod_pred_act
#' @description Check if the Schema in pre-production has the required fields
#' @param .schema the schema (see `?arize::create_schema`)
#' @param .environment integer, `1` for production, `2` for validation, `3` for training
#' @examples
#' s <- create_schema()
#' s$feature_column_names <- colnames(iris)[1:4]
#' s$prediction_id_column_name <- "Species"
#' check_existence_preprod_pred_act(s, 1)
#' \dontrun{
#' check_existence_preprod_pred_act(s, 2)
#' check_existence_preprod_pred_act(s, 3)
#' }
#'
#' # better when we add the required fields
#' s$prediction_label_column_name <- "Species"
#' s$actual_label_column_name <- "Iris"
#' check_existence_preprod_pred_act(s, 1)
#' check_existence_preprod_pred_act(s, 2)
#' check_existence_preprod_pred_act(s, 3)
#' @noRd
check_existence_preprod_pred_act <-
  function(.schema, .environment) {
    if (.environment %in% c(2, 3) &
        (is.null(.schema$prediction_label_column_name) ||
        is.null(.schema$actual_label_column_name))) {
      stop(
        "For logging pre-production data, the schema must specify both prediction and actual label columns."
      )
    }
  }

##### --- end parameter checks --- #####

##### --- type checks --- #####

#' check_type_prediction_id
#' @description Check the type of the prediction id column
#' @param .schema The schema (see `?arize::create_schema`)
#' @param .column_types named_list, with column names as names and column types as values
#' @details Allowed data types are character, numeric, and integer
#' @importFrom checkmate assert_choice
#' @examples
#' s <- create_schema(prediction_id_column_name = "Species")
#' col_types <- setNames(lapply(iris, class), names(iris))
#' \dontrun{check_type_prediction_id(s, col_types)}
#'
#' # making it a string, works
#' col_types$Species <- "character"
#' \dontrun{check_type_prediction_id(s, col_types)}
#' @noRd
check_type_prediction_id <- function(.schema, .column_types) {

  allowed_datatypes = c(
    arrow::string()$ToString(),
    arrow::int64()$ToString(),
    arrow::int32()$ToString(),
    arrow::int16()$ToString(),
    arrow::int8()$ToString()
  )

  col <- .schema$prediction_id_column_name
  assert_choice(col, names(.column_types))
  assert_choice(x = .column_types[[col]],
                choices = allowed_datatypes,
                .var.name = "prediction id column type")
}

#' check_type_timestamp
#' @description Check that the time stamp is of compatible class
#' @param .schema The schema (see `?arize::create_schema`)
#' @param .column_types named_list, with column names as names and column types as values
#' @details Allowed data types are integer, numeric, date, and POSIXct
#' @importFrom checkmate assert_choice
#' @examples
#' s <- create_schema(timestamp_column_name = "Month")
#' col_types <- setNames(lapply(airquality, class), names(airquality))
#' \dontrun{check_type_timestamp(s, col_types)}
#'
#' col_types$Month <- "double"
#' check_type_timestamp(s, col_types)
#' @noRd
check_type_timestamp <- function(.schema, .column_types) {

  allowed_datatypes = c(
    arrow::float64()$ToString(),
    arrow::int64()$ToString(),
    arrow::date32()$ToString(),
    arrow::date64()$ToString()
  )

  col <- .schema$timestamp_column_name
  assert_choice(col, names(.column_types))
  assert_choice(x = .column_types[[col]],
                choices = allowed_datatypes,
                .var.name = "timestamp column type")
}

#' check_type_features
#' @description Check that the feature columns are of correct type
#' @param .schema The schema (see `?arize::create_schema`)
#' @param .column_types named_list, with column names as names and column types as values
#' @details Allowed data types are integer, numeric, character, logical
#' @importFrom checkmate assert_choice
#' @examples
#' s <- create_schema(feature_column_names = names(iris)[1:4])
#' col_types <- setNames(lapply(iris, class), names(iris))
#' \dontrun{check_type_features(s, col_types)}
#'
#' col_types2 <- c(rep("double", 4), "string")
#' names(col_types2) <- names(iris)
#' check_type_features(s, col_types2)
#' @noRd
check_type_features <- function(.schema, .column_types) {
  allowed_datatypes = c(
    arrow::float64()$ToString(),
    arrow::int64()$ToString(),
    arrow::string()$ToString(),
    arrow::bool()$ToString(),
    arrow::int32()$ToString(),
    arrow::float32()$ToString(),
    arrow::int16()$ToString(),
    arrow::int8()$ToString()
  )

  lapply(.schema$feature_column_names,
         assert_choice,
         names(.column_types))
  lapply(
    .schema$feature_column_names,
    \(x) assert_choice(
      x = .column_types[[x]],
      choices = allowed_datatypes,
      .var.name = glue::glue("type of {x}")
    )
  )
}

#' check_type_shap_values
#' @description Check that the SHAP columns are of correct type
#' @param .schema The schema (see `?arize::create_schema`)
#' @param .column_types named_list, with column names as names and column types as values
#' @details Allowed data types are integer and numeric
#' @importFrom checkmate assert_choice
#' @examples
#' s <- create_schema(feature_column_names = names(iris)[1:4])
#' col_types <- setNames(lapply(iris, class), names(iris))
#' \dontrun{check_type_shap_values(s, col_types)}
#'
#' col_types2 <- c(rep("double", 4), "string")
#' names(col_types2) <- names(iris)
#' check_type_shap_values(s, col_types2)
#' @noRd
check_type_shap_values <- function(.schema, .column_types) {

  allowed_datatypes = c(
    arrow::float64()$ToString(),
    arrow::int64()$ToString(),
    arrow::float32()$ToString(),
    arrow::int32()$ToString()
  )

  invisible(lapply(.schema$shap_values_column_names,
         assert_choice,
         names(.column_types)))
invisible(lapply(
    ## TODO: is this a list of lists?
    .schema$shap_values_column_names,
    \(x) assert_choice(
      x = .column_types[[x]],
      choices = allowed_datatypes,
      .var.name = glue::glue("type of {x}")
    )
  ))
}

#' helper
#' @noRd
assert_column_and_type <-
  function(col_list,
           .column_types,
           allowed_datatypes) {
    for (L in col_list) {
      lapply(L,
             \(x) checkmate::assert_choice(
               x = x,
               choices = names(.column_types),
               .var.name = glue::glue("column {x}")
             ))
      lapply(
        L,
        \(x) checkmate::assert_choice(
          x = .column_types[[x]],
          choices = allowed_datatypes,
          .var.name = glue::glue("type of {x}")
        )
      )
    }
  }

#' check_type_pred_act_labels
#' @description Check prediction and actual label columns are of valid type for the
#' given model type
#' @param .model_type The type of the model, see the `arize::model_types` data object
#' @param .schema The schema (see `?arize::create_schema`)
#' @param .column_types named list, with column names as names and column types as values
#' @examples
#' # categorical or score-categorical models
#' iris2 <- iris
#' iris2$Species <- as.character(iris2$Species)
#' iris2$Label <- iris2$Species
#' s <- create_schema(feature_column_names = names(iris)[1:4],
#'                    prediction_label_column_name = "Species",
#'                    actual_label_column_name = "Label")
#' ct <- setNames(lapply(iris2, class), names(iris2))
#' ct$Species <- "string"
#' ct$Label <- "string"
#' \dontrun{
#'   check_type_pred_act_labels(.model_type = 1, .schema = s, .column_types = ct)
#'   check_type_pred_act_labels(.model_type = 2, .schema = s, .column_types = ct)
#' }
#' check_type_pred_act_labels(.model_type = 3, .schema = s, .column_types = ct)
#' check_type_pred_act_labels(.model_type = 4, .schema = s, .column_types = ct)
#' @noRd
check_type_pred_act_labels <-
  function(.model_type, .schema, .column_types) {
    col_list <- list(.schema$prediction_label_column_name,
                     .schema$actual_label_column_name)

    if (.model_type == model_types$BINARY) {
      allowed_datatypes <- arrow::bool()$ToString()
      assert_column_and_type(
        col_list = col_list,
        .column_types = .column_types,
        allowed_datatypes = allowed_datatypes
      )
    }

    if (.model_type == model_types$NUMERIC) {
      allowed_datatypes <- c(
        arrow::float64()$ToString(),
        arrow::int64()$ToString(),
        arrow::float32()$ToString(),
        arrow::int32()$ToString(),
        arrow::int16()$ToString(),
        arrow::int8()$ToString()
      )
      assert_column_and_type(
        col_list = col_list,
        .column_types = .column_types,
        allowed_datatypes = allowed_datatypes
      )
    }

    if (.model_type == model_types$CATEGORICAL) {
      allowed_datatypes <- arrow::string()$ToString()
      assert_column_and_type(
        col_list = col_list,
        .column_types = .column_types,
        allowed_datatypes = allowed_datatypes
      )
    }

    if (.model_type == model_types$SCORE_CATEGORICAL) {
      allowed_datatypes <- c(
        arrow::string()$ToString(),
        arrow::int64()$ToString(),
        arrow::bool()$ToString(),
        arrow::float64()$ToString(),
        arrow::int32()$ToString(),
        arrow::float32()$ToString(),
        arrow::int16()$ToString(),
        arrow::int8()$ToString()
      )

      assert_column_and_type(
        col_list = col_list,
        .column_types = .column_types,
        allowed_datatypes = allowed_datatypes
      )
    }
  }

#' check_type_pred_act_scores
#' @description Check prediction and actual score columns are of valid type for the
#' given model type
#' @param .model_type The type of the model, see the `arize::model_types` data object
#' @param .schema The schema (see `?arize::create_schema`)
#' @param .column_types named list, with column names as names and column types as values
#' @details Allowed datatypes are available though the `arize::model_types_allowed_datatypes`
#' data object
#' @examples
#'# categorical or score-categorical models
#' iris2 <- iris
#' iris2$Sepal.Length.Score <- sample(100, nrow(iris2), replace = TRUE)
#' s <- create_schema(feature_column_names = names(iris)[1:4],
#'                    prediction_score_column_name = "Sepal.Length",
#'                    actual_score_column_name = "Sepal.Length.Score")
#' ct <- setNames(lapply(iris2, class), names(iris2))
#' ct$Sepal.Length <- "double"
#' ct$Sepal.Length.Score <- "double"
#' # only relevant for SCORE_CATEGORICAL models
#' check_type_pred_act_scores(.model_type = 1, .schema = s, .column_types = ct)
#' check_type_pred_act_scores(.model_type = 2, .schema = s, .column_types = ct)
#' check_type_pred_act_scores(.model_type = 3, .schema = s, .column_types = ct)
#' \dontrun{check_type_pred_act_scores(.model_type = 4, .schema = s, .column_types = ct)}
#' @noRd
check_type_pred_act_scores <-
  function(.model_type, .schema, .column_types) {
    col_list <- list(.schema$prediction_score_column_name,
                     .schema$actual_score_column_name)

    if (.model_type == model_types$SCORE_CATEGORICAL) {
      allowed_datatypes = c(
        arrow::float64()$ToString(),
        arrow::int64()$ToString(),
        arrow::float32()$ToString(),
        arrow::int32()$ToString(),
        arrow::int16()$ToString(),
        arrow::int8()$ToString()
      )
      assert_column_and_type(
        col_list = col_list,
        .column_types = .column_types,
        allowed_datatypes = allowed_datatypes
      )
    }
  }

##### --- end type checks --- #####

##### --- value checks --- #####

#' check_value_timestamp
#' @description Validate the timestamp column in the schema
#' @param .data_frame a data frame
#' @param .schema the schema (see `?arize::Schema`)
#' @importFrom lubridate now years
#' @importFrom checkmate assert_null assert_choice assert_data_frame assert_numeric
#' @examples
#' iris2 <- iris
#' iris2$Timestamp_posixct <- Sys.time()
#' iris2$Timestamp_date <- Sys.Date()
#' iris2$Timestamp_int <- as.integer(Sys.time())
#' iris2$Timestamp_num <- as.numeric(Sys.time())
#'
#' # datetime
#' iris_schema <- create_schema(
#'   feature_column_names = names(iris2)[1:4],
#'   prediction_id_column_name = names(iris2)[5],
#'   timestamp_column_name = "Timestamp_posixct"
#' )
#'
#' check_value_timestamp(.data_frame = iris2, .schema = iris_schema)
#'
#' # date
#' iris_schema$timestamp_column_name <- "Timestamp_date"
#' check_value_timestamp(.data_frame = iris2, .schema = iris_schema)
#'
#' # integer
#' iris_schema$timestamp_column_name <- "Timestamp_int"
#' check_value_timestamp(.data_frame = iris2, .schema = iris_schema)
#'
#' # numeric
#' iris_schema$timestamp_column_name <- "Timestamp_num"
#' check_value_timestamp(.data_frame = iris2, .schema = iris_schema)
#' @noRd
check_value_timestamp <- function(.data_frame, .schema) {
  col <- .schema$timestamp_column_name

  assert_character(col, .var.name = "time stamp column name")
  assert_choice(col, colnames(.data_frame), .var.name = ".data_frame column names")
  assert_data_frame(.data_frame, min.rows = 1, .var.name = ".data_frame row number")
  assert_numeric(.data_frame[[col]], any.missing = FALSE, .var.name = "time stamp column type and content")
  bounds <- c(start = now() - years(1), end = now() + years(1))
  stats <- c(min = min(.data_frame[[col]]), max = max(.data_frame[[col]]))

  if (checkmate::test_posixct(.data_frame[[col]])) {
    stopifnot("Only values within one year from the current time are accepted." = bounds[[1]] < stats[[1]])
    stopifnot("Only values within one year from the current time are accepted." = bounds[[2]] > stats[[2]])
  }

  if (checkmate::test_date(.data_frame[[col]])) {
    bounds <- as.Date(bounds)
    stopifnot("Only values within one year from the current time are accepted." = bounds[[1]] < stats[[1]])
    stopifnot("Only values within one year from the current time are accepted." = bounds[[2]] > stats[[2]])
  }

  if (checkmate::test_integer(.data_frame[[col]])) {
    bounds <- as.integer(bounds)
    stopifnot("Only values within one year from the current time are accepted." = bounds[[1]] < stats[[1]])
    stopifnot("Only values within one year from the current time are accepted." = bounds[[2]] > stats[[2]])
  }

  if (checkmate::test_numeric(.data_frame[[col]])) {
    bounds <- as.numeric(bounds)
    stopifnot("Only values within one year from the current time are accepted." = bounds[[1]] < stats[[1]])
    stopifnot("Only values within one year from the current time are accepted." = bounds[[2]] > stats[[2]])
  }
}

#' check_value_missing
#' @description Validate the the prediction and actual id/label columns do not contain any missing data
#' @param .data_frame a data frame
#' @param .schema the schema (see `?arize::create_schema`)
#' @importFrom checkmate assert_numeric assert_logical
#' @examples
#'
#' iris2 <- iris
#' iris2$Species2 <- as.numeric(iris$Species)
#' iris2$Iris <- iris2$Species2
#' iris2$Observed <- iris2$Species2
#'
#' # datetime
#' iris_schema <- create_schema(
#'   feature_column_names = names(iris2)[1:4],
#'   prediction_id_column_name = "Species2",
#'   prediction_label_column_name = "Iris",
#'   actual_label_column_name = "Observed"
#' )
#'
#' check_value_missing(.data_frame = iris2, .schema = iris_schema)
#'
#' # add some missing data
#' iris2$Iris[[5]] <- NA_real_
#' \dontrun{check_value_missing(.data_frame = iris2, .schema = iris_schema)}
#' @noRd
check_value_missing <- function(.data_frame, .schema) {
  cols = c(
    prediction_id_column_name = .schema$prediction_id_column_name,
    prediction_label_column_name = .schema$prediction_label_column_name,
    actual_label_column_name = .schema$actual_label_column_name
  )

  for (i in seq_along(cols)) {
    if (any(is.na( .data_frame[[ cols[[i]] ]] ))) {
      stop(paste(names(cols)[[i]], "cannot contain missing data"))
    }

    if (any(is.null( .data_frame[[ cols[[i]] ]] ))) {
      stop(paste(names(cols)[[i]], "cannot contain null values"))
    }

    if (is.numeric( .data_frame[[ cols[[i]] ]] )) {
      if (any(is.infinite(.data_frame[[ cols[[i]] ]]))) {
        stop(paste(names(cols)[[i]], "cannot contain infinite values"))
      }
    }
  }

}

##### --- end value checks --- #####


##### --- validate parameters --- #####
#' validate_params
#' @description Wrapper to run parameter validation functions
#' @param .data_frame the data
#' @param .schema the schema (see `?arize::create_schema`)
#' @param .model_id character, the id of the model
#' @param .model_type The type of the model, see the `arize::model_types` data object
#' @param .model_version character, the model version
#' @param .environment integer, `1` for production, `2` for validation, `3` for training
#' @param .batch_id character, the batch id
#' @export
validate_params <- function(.data_frame,
                            .model_id,
                            .model_type,
                            .environment,
                            .schema,
                            .model_version,
                            .batch_id) {
  check_invalid_model_id(.model_id)
  check_invalid_model_version(.model_version, .environment)
  check_invalid_model_type(.model_type)
  check_invalid_environment(.environment)
  check_invalid_batch_id(.batch_id, .environment)
  check_existence_pred_act_shap(.schema)
  check_existence_preprod_pred_act(.schema, .environment)
  check_missing_columns(.data_frame, .schema)
}

##### --- end validate parameters --- #####

##### --- validate types --- #####
#' validate_types
#' @description Wrapper to run type validation functions
#' @param .schema the schema (see `?arize::create_schema`)
#' @param .model_type The type of the model, see the `arize::model_types` data object
#' @param .arrow_schema named_list, with column names as names and column types as values
#' @export
validate_types <- function(.model_type, .schema, .arrow_schema) {

  check_type_prediction_id(.schema, .arrow_schema)
  check_type_timestamp(.schema, .arrow_schema)
  check_type_features(.schema, .arrow_schema)
  check_type_shap_values(.schema, .arrow_schema)
  check_type_pred_act_labels(.model_type, .schema, .arrow_schema)
  check_type_pred_act_scores(.model_type, .schema, .arrow_schema)
}

##### --- end validate types --- #####

##### --- validate values --- #####
#' validate_values
#' @description Wrapper to run value validation functions
#' @param .data_frame the data
#' @param .schema the schema (see `?arize::create_schema`)
#' @export
validate_values <- function(.data_frame, .schema) {
  check_value_timestamp(.data_frame, .schema)
  check_value_missing(.data_frame, .schema)
}
##### --- end validate values --- #####

##### --- Validator class --- #####
#' Validator
#' @description R6 class to validate data frame and schema
#' @examples
#' I <- iris
#' I$Species_predicted_label <- I$Species
#' I$Species_predicted_score <- 1:150
#' I$Species_actual_label <- I$Species
#' I$Species_actual_score <- 150:1
#' I$Timestamp <- Sys.Date()
#'
#' S <- create_schema(
#'   prediction_id_column_name = "Species",
#'   feature_column_names = colnames(I)[1:4],
#'   timestamp_column_name = "Timestamp",
#'   prediction_label_column_name = "Species_predicted_label",
#'   prediction_score_column_name = "Species_predicted_score",
#'   actual_label_column_name = "Species_actual_label",
#'   actual_score_column_name = "Species_actual_score"
#' )
#'
#' V <- Validator$new()
#' V$validateParameters(
#'   .data_frame = I,
#'   .model_id = "1",
#'   .model_type = model_types$CATEGORICAL,
#'   .environment = environments$TRAINING,
#'   .schema = S,
#'   .model_version = "1",
#'   .batch_id = "1"
#' )
#' @export
Validator <- R6::R6Class(
  classname = "Validator",
  public = list(
    #' @description Run parameter validation functions
    #' @param ... Named arguments sent to `validate_params` (see `?validate_params`)
    validateParameters = function(...)
      validate_params(...),
    #' @description Run parameter validation functions
    #' @param ... Named arguments sent to `validate_types` (see `?validate_types`)
    validateTypes = function(...)
      validate_types(...),
    #' @description Run parameter validation functions
    #' @param ... Named arguments sent to `validate_values` see( `?validate_values`)
    validateValues = function(...)
      validate_values(...)
  )
)

##### --- end Validator class --- #####
