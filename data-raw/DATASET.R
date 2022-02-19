schema_fields <- list(
  "prediction_id_column_name",
  "feature_column_names",
  "timestamp_column_name",
  "prediction_label_column_name",
  "prediction_score_column_name",
  "actual_label_column_name",
  "actual_score_column_name",
  "shap_values_column_names"
)

model_types <- list(
  BINARY = 1,
  NUMERIC = 2,
  CATEGORICAL = 3,
  SCORE_CATEGORICAL = 4
)

environments <- list(PRODUCTION = 1,
                     VALIDATION = 2,
                     TRAINING = 3)


usethis::use_data(
  internal = FALSE,
  overwrite = TRUE,
  schema_fields,
  model_types,
  environments
)
