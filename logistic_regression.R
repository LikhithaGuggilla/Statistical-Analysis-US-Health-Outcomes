# This script focuses on building and evaluating logistic regression models to classify HighBloodPressure prevalence (above/below median).

model_logistic_regression <- function(merged_df) {

  # Create Table2_logreg: subset of merged_df for logistic regression.
  Table2_logreg <- merged_df[,c(11,16:30)]
  
  # Calculate the median of HighBloodPressure to create a binary target variable.
  MED <- median(Table2_logreg$HighBloodPressure, na.rm = TRUE)
  cat("Median proportion with HighBloodPressure:", MED, "\n")
  
  # Create a binary 'Flag' column: 1 if HighBloodPressure > MED, 0 otherwise.
  # This 'Flag' will be the target variable for logistic regression.
  Table2_logreg$Flag <- as.factor(as.numeric(Table2_logreg$HighBloodPressure > MED))
  # table(Table2_logreg$Flag) # Check distribution of 0s and 1s.
  
  # --- Logistic Regression - Full Model ---
  # Fit a logistic regression model using all predictors (except original HighBloodPressure) to predict 'Flag'.
  fit.full_log <- glm(Flag ~ . - HighBloodPressure, family='binomial', data=Table2_logreg)
  # summary(fit.full_log) 
  
  # Get predicted probabilities and classify based on a 0.5 threshold.
  preds_prob_full_log <- predict(fit.full_log, Table2_logreg, type='response') 
  preds_class_full_log <- ifelse(preds_prob_full_log > 0.5, 1, 0) 
  # Ensure actual 'Flag' is numeric (0/1) for metrics calculation.
  actual_log <- as.numeric(as.character(Table2_logreg$Flag)) 
  
  # conf_matrix_full_log <- table(Actual = actual_log, Predicted = preds_class_full_log)
  # print(conf_matrix_full_log) # Confusion matrix
  
  # Calculate and print error metrics for the full logistic model.
  # AUC (Area Under Curve) uses predicted probabilities.
  cat("Full Logistic Model - Accuracy:", accuracy(actual_log, preds_class_full_log), 
      "Recall:", recall(actual_log, preds_class_full_log), 
      "Precision:", precision(actual_log, preds_class_full_log), 
      "AUC:", auc(actual_log, preds_prob_full_log), 
      "AIC:", AIC(fit.full_log), "\n\n")
  
  # --- Logistic Regression - Reduced Model (Stepwise and p-hacking) ---
  # Perform stepwise variable selection using AIC to find a more parsimonious model.
  # trace = FALSE suppresses detailed step output.
  fit.stepwise_log <- stepAIC(fit.full_log, trace = FALSE) 
  # summary(fit.stepwise_log) # Uncomment for detailed summary of stepwise model.

  # P-hacking (manual removal based on p-values) was done in the notebook.
  # The notebook concluded that stepwise and p-hacking resulted in the same final model.
  # We will use the 'fit.stepwise_log' as the final reduced model.
  model_logistic_final <- fit.stepwise_log
    
  # Get predictions from the final reduced logistic model.
  preds_prob_stepwise_log <- predict(model_logistic_final, Table2_logreg, type='response')
  preds_class_stepwise_log <- ifelse(preds_prob_stepwise_log > 0.5, 1, 0)
  
  # conf_matrix_stepwise_log <- table(Actual = actual_log, Predicted = preds_class_stepwise_log)
  # print(conf_matrix_stepwise_log) # Confusion matrix for reduced model
  
  # Calculate and print error metrics for the reduced (stepwise) logistic model.
  cat("Stepwise Logistic Model - Accuracy:", accuracy(actual_log, preds_class_stepwise_log), 
      "Recall:", recall(actual_log, preds_class_stepwise_log), 
      "Precision:", precision(actual_log, preds_class_stepwise_log), 
      "AUC:", auc(actual_log, preds_prob_stepwise_log), 
      "AIC:", AIC(model_logistic_final), "\n")
  
  
  return(list(final_model=model_logistic_final, 
              predictions_df=cbind(Table2_logreg, PredProb_Logistic=preds_prob_stepwise_log, PredClass_Logistic=preds_class_stepwise_log)))
}