
# This script handles the initial setup for the analysis.

# Package installations 
# This section is for installing necessary R packages if they are not already installed.
# install.packages(c("dplyr", "ggplot2", "corrgram", "psych", "MASS", "car", "Metrics"))

# Load libraries
# These libraries provide functions for data manipulation, plotting, statistical analysis, and model evaluation.
library(dplyr)    # For data manipulation (e.g., %>%, group_by, summarise)
library(ggplot2)  # For creating a wide variety of plots
library(corrgram) # For plotting correlograms
library(psych)    # For various psychological and statistical functions (used in EDA)
library(MASS)     # For functions and datasets from Modern Applied Statistics with S (e.g., stepAIC)
library(car)      # For Companion to Applied Regression functions (e.g., vif, powerTransform)
library(Metrics)  # For model evaluation metrics (e.g., rmse, mae, mape, auc)

