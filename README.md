
<div align="center">

<img src="https://storage.googleapis.com/arize-assets/arize-logo-white.jpg" width="600" /><br><br>

</div>

[![Slack](https://img.shields.io/badge/slack-@arize-yellow.svg?logo=slack)](https://join.slack.com/t/arize-ai/shared_invite/zt-g9c1j1xs-aQEwOAkU4T2x5K8cqI1Xqg)

# arize

## Overview

A helper library to interact with Arize AI APIs.

Arize is an end-to-end ML observability and model monitoring platform.
The platform is designed to help ML engineers and data science
practitioners surface and fix issues with ML models in production faster
with: - Automated ML monitoring and model monitoring - Workflows to
troubleshoot model performance - Real-time visualizations for model
performance monitoring, data quality monitoring, and drift monitoring -
Model prediction cohort analysis - Pre-deployment model validation -
Integrated model explainability

## Installation

You can install the development version of arize from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("Arize-ai/client_r")
```

## Example

In this example we are going to log modelling results for a
click-through rate calculation. For background on the data and model,
please refer to the original documentation (TODO: LINK). Here we
download the training, validation, and production datasets to show how
to use the `arize` package to log modelling results to Arize AI from
`R`.

Logging to Arize AI in three steps:

1.  Perform your analysis and store results you wish to start logging
    (in this example we start with already prepared datasets)
2.  Create a logging schema. This is a named list of parameters that
    describes the dataset you are logging
3.  Initiate an `arize_logger` instance to send data to the Arize AI API
    (validation of the dataset and schema is carried out internally)

### Step 1: Model results

For this vignette, we download datasets with model results to log. In
your usage, this step would be replaced with your modelling procedures.

``` r
data_url <-
  "https://storage.googleapis.com/arize-assets/fixtures/Click-Through%20Rate%20Use-Case/click_through_rate_categorical_"
dataset_names <- c("training", "validation", "production")
datasets <- lapply(dataset_names,
                   function(x)
                     read.csv(paste(data_url, x, ".csv", sep = "")))
datasets <- setNames(datasets, dataset_names)
```

For clarity, we can separate the `datasets` objects into three data
frames, for training, validation, and production. We also make sure the
results timestamp is an vector of class `Date` as required by the Arize
AI API.

``` r
# This is the data which we will be logging
df_train <- datasets[["training"]]
df_train$model_date <- as.Date(df_train$model_date)
df_valid <- datasets[["validation"]]
df_valid$model_date <- as.Date(df_valid$model_date)
df_prod <- datasets[["production"]]
df_prod$model_date <- as.Date(df_prod$model_date)
```

### Step 2: Schema

The `schema` is a simple named list describing the data you wish to log.
The possible fields in the schema can be seen by looking up the
`schema_fields` data object exported by the `arize` package. To create a
schema we???ll use the `arize::create_schema` function. We first declare
our model id, model version, and create a vector of the features.
Thereafter, we pass these as arguments to `create_schema`.

``` r
model_id <- "click_through_rate_categorical_vignette"  # This is the model name that will show up in Arize
model_version <- "v1.0"  # Version of model - can be any string

features <- c(
  "position",
  "domain",
  "category",
  "device",
  "keywords"
)

# Define a Schema() object for Arize to pick up data from the correct columns for logging
library(arize)
schema <- create_schema(
  prediction_id_column_name = "id",
  prediction_label_column_name = "predictions",
  prediction_score_column_name = "CTR_predicted",
  actual_label_column_name = "actuals",
  actual_score_column_name = "CTR",
  feature_column_names = features,
  timestamp_column_name = "model_date"
)

schema
#> $prediction_id_column_name
#> [1] "id"
#>
#> $prediction_label_column_name
#> [1] "predictions"
#>
#> $prediction_score_column_name
#> [1] "CTR_predicted"
#>
#> $actual_label_column_name
#> [1] "actuals"
#>
#> $actual_score_column_name
#> [1] "CTR"
#>
#> $feature_column_names
#> [1] "position" "domain"   "category" "device"   "keywords"
#>
#> $timestamp_column_name
#> [1] "model_date"
```

### Step 3: Log to Arize AI

We begin by starting a new instance of the `arize`???s logger (see
`?arize::Client`). For this, we need our authentication credentials that
we can look up from the `Space settings` on our Arize AI dashboard. We
can pass these keys to the logger directly in our script, or store them
as environmental variables in an `.Renviron` file. In the latter case,
the we can initiate a new `arize::Client` instance without supplying
these parameters. Below, we show both approaches:

``` r
# Keys added in the code
ORGANIZATION_KEY <- "your organization key"
API_KEY <- "your api key"
arize_client <- Client$new(organization_key = ORGANIZATION_KEY, api_key = API_KEY)
```
--- or ---
``` r
# Keys stored in `.Renviron`
arize_client <- Client$new()
```

With our Arize logger instantiated, we can proceed to log data to the
server. We???ll need the meta data, schema, and model results data frame.
In addition, we also need to match the results data to the type of the
model and environment. To see the available model types and
environments, look up the `arize::model_types` and `arize::environments`
data objects.

``` r
model_types
#> $BINARY
#> [1] 1
#>
#> $NUMERIC
#> [1] 2
#>
#> $CATEGORICAL
#> [1] 3
#>
#> $SCORE_CATEGORICAL
#> [1] 4

environments
#> $PRODUCTION
#> [1] 1
#>
#> $VALIDATION
#> [1] 2
#>
#> $TRAINING
#> [1] 3
```

We can now log the training, validation, and production results data.

Training data:

``` r
arize_client$log(
  .data_frame = df_train,
  .model_id = model_id,
  .model_version = model_version,
  .model_type = model_types$SCORE_CATEGORICAL,
  .environment = environments$TRAINING,
  .schema = schema
)
#> [1] ""
```

For the validation data, we also supply a batch id to the logger:

``` r
arize_client$log(
  .data_frame = df_valid,
  .model_id = model_id,
  .model_version = model_version,
  .batch_id = "validation",
  .model_type = model_types$SCORE_CATEGORICAL,
  .environment = environments$VALIDATION,
  .schema = schema
)
#> [1] ""
```

And production data:

``` r
arize_client$log(
  .data_frame = df_prod,
  .model_id = model_id,
  .model_version = model_version,
  .model_type = model_types$SCORE_CATEGORICAL,
  .environment = environments$PRODUCTION,
  .schema = schema
)
#> [1] ""
```

Note that we changed the `environment` passed to the model to match the
dataset being logged, but otherwise the calls to `arize_client$log` are
identical for the three modelling stages.

The logger function returns the result of a `httr::POST()` call that
sends our data to Arize AI. If everything went well, you should see an
empty string printed to the `R` console (`""`). If you receive some
other response from the API, things might have gone wrong. In such cases
please consult the [API
documentation](https://docs.arize.com/arize/data-ingestion/api-reference/rest-api#response-codes)
for more information.

``` r
sessionInfo()
```
