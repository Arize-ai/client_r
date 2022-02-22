# schema missing prediction label, actual label or SHAP column names
s <- Schema$new()
s$feature_column_names <- colnames(iris)[1:4]
s$prediction_id_column_name <- "Species"
tryCatch(check_existence_pred_act_shap(s))

# better when we add one of these
s$prediction_label_column_name <- "Species"
check_existence_pred_act_shap(s)
