#' validate character helper
#' @noRd
validate_character <- function(X) {
  if (!test_character(
    X,
    any.missing = FALSE,
    all.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )) {
    X <- as.character(X)
    assert_character(
      X,
      any.missing = FALSE,
      all.missing = FALSE,
      len = 1,
      null.ok = FALSE
    )
  }
}

#' Client
#'
#' @export
Client <- R6::R6Class(
  classname = "Client",
  public = list(
    #' @field api_key Arize API key
    api_key = Sys.getenv("ARIZE_API_KEY"),
    #' @field organization_key Arize organization key
    organization_key = Sys.getenv("ARIZE_ORGANIZATION_KEY"),
    #' @field files_uri Arize API URI
    files_uri = "https://api.arize.com/v1",

    #' @description Init a new client instance
    #' @param api_key character, the Arize-AI api key
    #' @param organization_key character, the Arize-AI organization key
    #' @param uri character, the API URI
    #' @details A check is made for environmental variables for the API and Organization keys
    #' via Sys.getenv("ARIZE_API_KEY") and Sys.getenv("ARIZE_ORGANIZATION_KEY"). To set these
    #' environmental variables, use, e.g., Sys.setenv("ARIZE_ORGANIZATION_KEY" = "YOUR_ARIZE_ORGANIZATION_KEY")
    #' or create an `.Renviron` file and list these two keys.
    initialize = function(api_key = Sys.getenv("ARIZE_API_KEY"),
                          organization_key = Sys.getenv("ARIZE_ORGANIZATION_KEY"),
                          uri = "https://api.arize.com/v1") {
      self$api_key <- api_key
      self$organization_key <- organization_key
      self$files_uri <- paste(uri, "/pandas_arrow", sep = "")
    },

    #' @description Post a serialized file
    #' @param .path path to the file to post
    #' @param .schema the schema (see `?arize::create_schema`)
    #' @param .sync logical, whether to sync
    post_file = function(.path, .schema, .sync) {

      headers <- c(
        "authorization" = self$api_key,
        "organization" = self$organization_key,
        "schema" = .schema,
        "sdk-version" = "0.1.0",
        "sdk" = "R"
      )

      if (.sync) {
        headers <- c(headers, sync = "1")
      }

      f <- httr::upload_file(.path)
      h <- httr::add_headers(.headers = headers)
      request <-
        httr::POST(
          url = self$files_uri,
          body = f,
          h, # headers are passed as unnamed argument
          encode = "multipart"
        )
      httr::content(x = request, as = "text", encoding = "UTF-8")
    },

    #' @description Log to Arize
    #' @param .data_frame data.frame to log
    #' @param .model_id character, id for the model
    #' @param .model_type integer, `1` for binary, `2` for numeric, `3` for categorical, and `4` for score-categorical
    #' @param .environment .environment integer, `1` for production, `2` for validation, `3` for training
    #' @param .schema the schema (see `?arize::create_schema`)
    #' @param .model_version character, the model version, optional
    #' @param .batch_id character, the batch id, optional
    # TODO: .sync description requires more context
    #' @param .sync logical, whether to sync
    #' @param .validate logical, whether to run validation checks
    #' @param .path character, path to use for serialization, optional
    #' @importFrom checkmate test_character assert_atomic assert_list assert_data_frame assert_logical
    #' @export
    log = function(.data_frame,
                   .model_id,
                   .model_type,
                   .environment,
                   .schema,
                   .model_version = NULL,
                   .batch_id = NULL,
                   .sync = FALSE,
                   .validate = TRUE,
                   .path = NULL) {
      # validate model id
      assert_atomic(.model_id)
      validate_character(.model_id)

      assert_atomic(.model_type)
      assert_atomic(.environment)
      assert_data_frame(.data_frame)
      assert_list(.schema)

      # validate model version (if relevant)
      if (!is.null(.model_version)) {
        assert_atomic(.model_version)
        validate_character(.model_version)
      }

      # validate batch id (if relevant)
      if (!is.null(.batch_id)) {
        assert_atomic(.batch_id)
        validate_character(.batch_id)
      }

      if (!is.null(.path))
        assert_atomic(.path)
      if (!is.null(.sync))
        assert_logical(.sync)

      if (!is.null(.validate)) {
        assert_logical(.validate)
        if (.validate) {
          validate_params(
            .data_frame = .data_frame,
            .model_id = .model_id,
            .model_type = .model_type,
            .environment = .environment,
            .schema = .schema,
            .model_version = .model_version,
            .batch_id = .batch_id
          )
        }
      }

      if (any(sapply(.data_frame, checkmate::test_class, "factor"))) {
        factors <- sapply(.data_frame, is.factor)
        .data_frame[factors] <-
          lapply(.data_frame[factors], as.character)
      }

      a_table <- tryCatch(
        arrow::arrow_table(.data_frame),
        error = function(e) {
          stop(
            glue::glue(
              "The dataframe needs to convert to pyarrow but has failed to do so. ",
              "There may be unrecognized data types in the dataframe. ",
              "Another reason may be that a column in the dataframe has a mix of strings and numbers, ",
              "in which case you may want to convert the strings in that column to NaN. ",
              "See https://docs.arize.com/arize/api-reference/python-sdk/arize.pandas/mixed-types"
            )
          )
        }
      )

      if (!is.null(.validate)) {
        assert_logical(.validate)
        if (.validate) {
          nms <- a_table$ColumnNames()
          a_schema <-
            vapply(
              X = nms,
              FUN = \(x) a_table$GetColumnByName(x)$type$ToString(),
              FUN.VALUE = character(1)
            )

          validate_types(
            .model_type = .model_type,
            .schema = .schema,
            .arrow_schema = a_schema
          )

          validate_values(.data_frame = .data_frame,
                          .schema = .schema)
        }
      }

      # populate proto schema
      arize.proto.dir <- system.file("proto", package = "arize")
      RProtoBuf::readProtoFiles(dir = arize.proto.dir)
      S <- RProtoBuf::new(Class = public.Schema)
      S$constants$model_id <- .model_id

      if (!is.null(.model_version)) {
        S$constants$model_version <- .model_version
      }

      S$constants$environment <-
        switch(
          .environment,
          public.Schema$Environment$PRODUCTION,
          public.Schema$Environment$VALIDATION,
          public.Schema$Environment$TRAINING
        )

      S$constants$model_type <-
        switch(
          .model_type,
          public.Schema$ModelType$BINARY,
          public.Schema$ModelType$NUMERIC,
          public.Schema$ModelType$CATEGORICAL,
          public.Schema$ModelType$SCORE_CATEGORICAL
        )

      if (!is.null(.batch_id)) {
        S$constants$batch_id <- .batch_id
      }

      S$arrow_schema$prediction_id_column_name <-
        .schema$prediction_id_column_name

      if (!is.null(.schema$timestamp_column_name)) {
        S$arrow_schema$timestamp_column_name = .schema$timestamp_column_name
      }

      if (!is.null(.schema$prediction_label_column_name)) {
        S$arrow_schema$prediction_label_column_name <-
          .schema$prediction_label_column_name
      }

      if (!is.null(.schema$prediction_score_column_name)) {
        S$arrow_schema$prediction_score_column_name <-
          .schema$prediction_score_column_name
      }

      if (!is.null(.schema$feature_column_names)) {
        S$arrow_schema$feature_column_names <- .schema$feature_column_names
      }

      if (!is.null(.schema$actual_label_column_name)) {
        S$arrow_schema$actual_label_column_name <-
          .schema$actual_label_column_name
      }

      if (!is.null(.schema$actual_score_column_name)) {
        S$arrow_schema$actual_score_column_name <-
          .schema$actual_score_column_name
      }

      if (!is.null(.schema$shap_values_column_names)) {
        SHAPS <- RProtoBuf::new(public.Schema$ArrowSchema$ShapValuesColumnNamesEntry)

        lapply(seq_along(.schema$shap_values_column_names), \(i) {
          SHAPS$key <- names(.schema$shap_values_column_names)[[i]]
          SHAPS$value <- .schema$shap_values_column_names[[i]]
        })
        S$arrow_schema$shap_values_column_names <- SHAPS
      }

      if (is.null(.path)) {
        tmp_file <- tempfile()
      } else {
        tmp_file <- .path
      }

      S$serialize(tmp_file)
      base64_schema <- base64enc::base64encode(tmp_file)
      arrow::write_ipc_stream(x = .data_frame, sink = tmp_file)

      self$post_file(
        .path = tmp_file,
        .schema = base64_schema,
        .sync = .sync
      )
    }
  )
)
