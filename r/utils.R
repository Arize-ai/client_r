#' infer_model_type
#'
#' @description Return the model type (Binary, Numeric, Categorical, Score Categorical)
#' based on the type (class) of the input variable
#' @param .vector The variable we wish to model
#'
#' @importFrom checkmate assert_choice
#'
#' @examples
#' infer_model_type(LETTERS)
#' infer_model_type(iris$Sepal.Length)
#' infer_model_type(iris$Species)
#' infer_model_type(sample(c(TRUE, FALSE), 10, replace=TRUE))
#'
#' # informative error messages
#' \dontrun{
#' infer_model_type(iris)
#' infer_model_type(as.list(LETTERS))
#' }
#' @noRd
infer_model_type <- function(.vector) {
  assert_choice(
    x = class(.vector),
    choices = c("logical", "numeric", "character", "factor")
  )

  ## TODO: is the tupple option an ordered factor (score categorical model?)
  switch(
    class(.vector),
    "logical" = model_types$BINARY,
    "numeric" = model_types$NUMERIC,
    "character" = model_types$CATEGORICAL,
    "factor" = model_types$SCORE_CATEGORICAL
  )
}
