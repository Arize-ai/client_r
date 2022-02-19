s <- Schema$new()
s$feature_column_names <- colnames(iris)[1:4]
s$prediction_id_column_name <- "Species"
check_existence_preprod_pred_act(s, 1)
tryCatch(check_existence_preprod_pred_act(s, 2))
tryCatch(check_existence_preprod_pred_act(s, 3))

# better when we add the required fields
s$prediction_label_column_name <- "Species"
s$actual_label_column_name <- "Iris"
check_existence_preprod_pred_act(s, 1)
check_existence_preprod_pred_act(s, 2)
check_existence_preprod_pred_act(s, 3)
