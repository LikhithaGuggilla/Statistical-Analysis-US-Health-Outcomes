# This script focuses on building and evaluating linear regression models to understand the unhealthy behaviour and lack of preventive measure contributing to High BP, predict HighBloodPressure proportions.

# Target variable = HighBloodPressure

model_linear_regression <- function(merged_df) {

  # Create Table2_linreg: subset of merged_df for linear regression,
  Table2_linreg <- merged_df[,c(11,16:30)]
  
  # Plot the density of the original HighBloodPressure target variable.
  # This helps to assess its distribution for normality, an assumption of linear regression.
  plot(density(Table2_linreg$HighBloodPressure), col="dark blue", lwd=3, main="Distribution of HighBloodPressure")
  
  # Use powerTransform to find an optimal transformation for normality.
  # summary(powerTransform(Table2_linreg$HighBloodPressure)) 
  
  # Plot the density of the transformed target variable (HighBloodPressure^-1).
  plot(density(Table2_linreg$HighBloodPressure^-1), col="dark blue", lwd=3, main="Distribution of TRANSFORMED HighBloodPressure (^-1)")
  
  # Standardize predictor variables (columns 2 to 16 of Table2_linreg) using Z-score scaling.
  # This brings all predictors to a similar scale, which can improve model insights and performance.
  dfZ_Score <- as.data.frame(scale(Table2_linreg[,c(2:16)])) 
  # Combine standardized predictors with the transformed target variable.
  Table2_linreg_std_transformed <- cbind(dfZ_Score, "HighBloodPressure_inv" = Table2_linreg$HighBloodPressure^-1)
  
  # Model 1 - Linear Regression with untransformed Target Variable and original predictors.
  fit.full1_lin <- lm(HighBloodPressure ~ . , data=Table2_linreg) 
  # summary(fit.full1_lin) # Uncomment for detailed model summary
  myRMSE_full1_lin <- rmse(Table2_linreg$HighBloodPressure, fit.full1_lin$fitted.values)
  myMAE_full1_lin <- mae(Table2_linreg$HighBloodPressure, fit.full1_lin$fitted.values)
  myMAPE_full1_lin <- mape(Table2_linreg$HighBloodPressure, fit.full1_lin$fitted.values)
  AIC_full1_lin <- AIC(fit.full1_lin)
  cat("Linear Model 1 (Untransformed Target) - RMSE:", myRMSE_full1_lin, "MAE:", myMAE_full1_lin, "MAPE:", myMAPE_full1_lin, "AIC:", AIC_full1_lin, "\n")
  
  # Model 2: Linear Regression with Transformed target variable (inverse) and original predictors.
  # The target is transformed within the formula using I().
  fit.full2_lin <- lm(I(HighBloodPressure^-1) ~ . - HighBloodPressure, data=Table2_linreg)
  # summary(fit.full2_lin) # Uncomment for detailed model summary
  myRMSE_full2_lin <- rmse(Table2_linreg$HighBloodPressure^-1, fit.full2_lin$fitted.values)
  myMAE_full2_lin <- mae(Table2_linreg$HighBloodPressure^-1, fit.full2_lin$fitted.values)
  myMAPE_full2_lin <- mape(Table2_linreg$HighBloodPressure^-1, fit.full2_lin$fitted.values)
  AIC_full2_lin <- AIC(fit.full2_lin)
  cat("Linear Model 2 (Transformed Target) - RMSE:", myRMSE_full2_lin, "MAE:", myMAE_full2_lin, "MAPE:", myMAPE_full2_lin, "AIC:", AIC_full2_lin, "\n")

  # Model 3: Linear Regression with standardized predictor variables and transformed target variable.
  fit.full3_lin <- lm(HighBloodPressure_inv ~ . , data=Table2_linreg_std_transformed)
  # summary(fit.full3_lin) # Uncomment for detailed model summary
  myRMSE_full3_lin <- rmse(Table2_linreg_std_transformed$HighBloodPressure_inv, fit.full3_lin$fitted.values)
  myMAE_full3_lin <- mae(Table2_linreg_std_transformed$HighBloodPressure_inv, fit.full3_lin$fitted.values)
  myMAPE_full3_lin <- mape(Table2_linreg_std_transformed$HighBloodPressure_inv, fit.full3_lin$fitted.values)
  AIC_full3_lin <- AIC(fit.full3_lin)
  cat("Linear Model 3 (Standardized, Transformed) - RMSE:", myRMSE_full3_lin, "MAE:", myMAE_full3_lin, "MAPE:", myMAPE_full3_lin, "AIC:", AIC_full3_lin, "\n")
  
  # VIF reduction (based on fit.full2_lin as selected in notebook for further refinement)
  # Check VIF for multicollinearity. VIF > 5 or 10 suggests potential issues.
  # print(vif(fit.full2_lin)) # Notebook indicated DentistVisit, NoPhysicalAcivity, RoutineChcekup > 5
  
  # Iteratively remove variables with high VIF.
  fit.vif1_lin <- lm(I(HighBloodPressure^-1) ~ . - HighBloodPressure - DentistVisit, data = Table2_linreg)
  # print(vif(fit.vif1_lin))
  fit.vif2_lin <- lm(I(HighBloodPressure^-1) ~ . - HighBloodPressure - DentistVisit - NoPhysicalAcivity, data = Table2_linreg)
  # print(vif(fit.vif2_lin))
  model_vif_reduced_lin <- lm(I(HighBloodPressure^-1) ~ . - HighBloodPressure - DentistVisit - NoPhysicalAcivity - RoutineChcekup, data = Table2_linreg)
  # print(vif(model_vif_reduced_lin)) # VIFs should be lower now.
  
  # Variable selection - p-hacking (based on model_vif_reduced_lin)
  # Further refine the model by removing variables with high p-values (low statistical significance).
  # summary(model_vif_reduced_lin) # Notebook observed Mammography and FecalOccultBloodTest had higher p-values
  
  # Iteration 1: Remove 'Mammography'
  fit.phack1_lin <- update(model_vif_reduced_lin, . ~ . - Mammography)
  
  # Iteration 2: Remove 'FecalOccultBloodTest' from the previously updated model.
  model_linear_final <- update(fit.phack1_lin, . ~ . - FecalOccultBloodTest)
  cat("\nFinal Linear Model (after VIF and p-value reduction) AIC:", AIC(model_linear_final), "\n")
  # summary(model_linear_final) # Uncomment for details of the final linear model
  
  # --- Visualization of Final Linear Model Performance ---
  actual_lin_inv <- Table2_linreg$HighBloodPressure^-1 # Actual transformed target values
  preds_lin_inv <- model_linear_final$fitted.values   # Predicted transformed target values
  
  # Plot actual vs. predicted values (on the transformed scale).
  # A good fit would show points clustered around the y=x line.
  plot(actual_lin_inv, preds_lin_inv, pch=19, col='red', cex=0.7, 
       main='Actual vs. Predicted (Transformed HighBP^-1)',
       xlab="Actual HighBP^-1", ylab="Predicted HighBP^-1")
  abline(0,1, lty=1, col='black', lwd=2) # y=x line for reference
  
  # Plot actual vs. residuals (on the transformed scale).
  # Residuals should be randomly scattered around zero, indicating no systematic error.
  plot(x = actual_lin_inv, y = model_linear_final$residuals,
       xlab = "Actual HighBP^-1", ylab = "Residuals",
       main = "Actual vs. Residuals (Transformed HighBP^-1)",
       col = 'blue', pch=19, cex=0.7)
  abline(h = 0, col = "red", lwd = 2) # Zero residual line
  
  # Store Predictions (back-transformed to original scale) in the main dataframe.
  merged_df_preds_lin <- cbind(merged_df, "BackTransformedLinPreds" = preds_lin_inv^-1)
  
  # Create a table for regional analysis of residuals.
  Table_regions_lin <- merged_df_preds_lin[,c(32,33,11,34)] # Columns: Regions, Divisions, Actual HighBP, Predicted HighBP
  colnames(Table_regions_lin) <- c("Regions","Divisions","Actual_HighBP","Predicted_HighBP_Lin")
  # Calculate residuals on the original scale.
  Table_regions_lin$Residuals_Lin <- Table_regions_lin$Actual_HighBP - Table_regions_lin$Predicted_HighBP_Lin
  
  # Boxplot of residuals by region (original scale).
  boxplot(Residuals_Lin ~ Regions, data = Table_regions_lin,
          main = "Linear Regression Residuals by Region",
          xlab = "Region", ylab = "Residuals (Actual - Predicted HighBP)")
  
  # Calculate Mean Absolute Error (MAE) by region.
  mae_data_lin_region <- tapply(abs(Table_regions_lin$Residuals_Lin), Table_regions_lin$Regions, mean, na.rm=TRUE)
  mae_by_region_lin <- data.frame(Regions = names(mae_data_lin_region), MAE_Lin = mae_data_lin_region)
  print(mae_by_region_lin)
  
  # Boxplot of residuals by division (original scale).
  par(mar=c(10, 4, 4, 2) + 0.1) # Adjust margins for rotated labels
  boxplot(Residuals_Lin ~ Divisions, data = Table_regions_lin,
          main = "Linear Regression Residuals by Division",
          ylab = "Residuals (Actual - Predicted HighBP)", las=2, cex.axis=0.7) # las=2 for perpendicular labels
  par(mar=c(5, 4, 4, 2) + 0.1) # Reset margins
  
  # Calculate MAE by division.
  mae_data_lin_division <- tapply(abs(Table_regions_lin$Residuals_Lin), Table_regions_lin$Divisions, mean, na.rm=TRUE)
  mae_by_division_lin <- data.frame(Divisions = names(mae_data_lin_division), MAE_Lin = mae_data_lin_division)
  print(mae_by_division_lin)
  
  return(list(final_model=model_linear_final, predictions_df=merged_df_preds_lin))
}