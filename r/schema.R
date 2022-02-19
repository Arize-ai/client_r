#' Schema
#' @description Create a Schema list
#' @param .list A named list of key-value pairs for schema fields
#' @param ... Key-value pairs of schema fields
#' @details Field options are:
#'  - prediction_id_column_name
#'  - feature_column_names*
#'  - timestamp_column_name
#'  - prediction_label_column_name
#'  - prediction_score_column_name
#'  - actual_label_column_name
#'  - actual_score_column_name
#'  - shap_values_column_names*
#'
#'  Fields denoted with a star are lists of column names
#'
#' @examples
#' create_schema(timestamp_column_name = "timestamp")
#' create_schema(.list = list(timestamp_column_name = "timestamp"))
#' create_schema(
#'   .list = list(timestamp_column_name = "timestamp"),
#'   prediction_id_column_name = "predict_id"
#' )
#' create_schema(
#'   feature_column_names = c(colnames(iris)[1:4], "Stamen.Count", "Anther.Length"),
#'   prediction_id_column_name = "Species"
#' )
#' @importFrom checkmate assert_choice
#' @export
create_schema <- function(.list = NULL, ...) {
  lapply(names(.list), assert_choice, unlist(schema_fields))
  lapply(names(list(...)), assert_choice, unlist(schema_fields))
  c(.list, list(...))
}
