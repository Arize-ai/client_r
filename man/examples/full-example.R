# some modelling data
data_url <-
  "https://storage.googleapis.com/arize-assets/fixtures/Click-Through%20Rate%20Use-Case/click_through_rate_categorical_"
dataset_names <- c("training", "validation", "production")
datasets <- lapply(dataset_names,
                   function(x)
                     read.csv(paste(data_url, x, ".csv", sep = "")))
datasets <- setNames(datasets, dataset_names)

# average CTR:
(100 * sum(datasets[["production"]][["CTR_predicted"]]) / nrow(datasets[["production"]]))

model_id <- "R_click_through_rate_categorical"  # This is the model name that will show up in Arize
model_version <- "v1.0"  # Version of model - can be any string

# This is the data which we will be logging
df_train <- datasets[["training"]]
df_train$model_date <- as.Date(df_train$model_date)
df_valid <- datasets[["validation"]]
df_valid$model_date <- as.Date(df_valid$model_date)
df_prod <- datasets[["production"]]
df_prod$model_date <- as.Date(df_prod$model_date)

features <- c(
  "position",
  "domain",
  "category",
  "device",
  "keywords"
)

# Define a Schema() object for Arize to pick up data from the correct columns for logging
schema <- arize::create_schema(
  prediction_id_column_name = "id",
  prediction_label_column_name = "predictions",
  prediction_score_column_name = "CTR_predicted",
  actual_label_column_name = "actuals",
  actual_score_column_name = "CTR",
  feature_column_names = features,
  timestamp_column_name = "model_date"
)

# start a new logger
ORGANIZATION_KEY <- ""
API_KEY <- ""

library(arize)
arize_client <- Client$new(organization_key = ORGANIZATION_KEY, api_key = API_KEY)

# Logging Training DataFrame
arize_client$log(
  .data_frame = df_train,
  .model_id = model_id,
  .model_version = model_version,
  .model_type = model_types$SCORE_CATEGORICAL,
  .environment = environments$TRAINING,
  .schema = schema
)

# Logging Validation DataFrame
arize_client$log(
  .data_frame = df_valid,
  .model_id = model_id,
  .model_version = model_version,
  .batch_id = "validation",
  .model_type = model_types$SCORE_CATEGORICAL,
  .environment = environments$VALIDATION,
  .schema = schema,
)

# Define a Schema() object for Arize to pick up data from the correct columns for logging
schema_prod <- arize::create_schema(
  prediction_id_column_name = "id",
  prediction_label_column_name = "predictions",
  prediction_score_column_name = "CTR_predicted",
  actual_label_column_name = "actuals",
  actual_score_column_name = "CTR",
  feature_column_names = features,
  timestamp_column_name = "model_date",
)

# arize_client.log returns a Response object from Python's requests module
arize_client$log(
  .data_frame = df_prod,
  .model_id = model_id,
  .model_version = model_version,
  .model_type = model_types$SCORE_CATEGORICAL,
  .environment = 1,
  .schema = schema_prod,
)
