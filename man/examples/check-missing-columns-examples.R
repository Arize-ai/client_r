# all good
s <- Schema$new()
s$feature_column_names <- colnames(iris)[1:4]
s$prediction_id_column_name <- "Species"
check_missing_columns(.data_frame = iris, .schema = s)

# column from schema not found in data frame
s <- Schema$new()
s$feature_column_names <- c(colnames(iris)[1:4], "Stamen.Count", "Anther.Length")
s$prediction_id_column_name <- "Species"
tryCatch(check_missing_columns(.data_frame = iris, .schema = s))
