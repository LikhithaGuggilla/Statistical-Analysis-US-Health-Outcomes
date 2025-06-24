# This script performs one-sample and two-sample t-tests based on EDA findings.

perform_hypothesis_tests <- function(merged_df) {
  
  # --- One Sample T-Test ---
  # Objective: Test if the average HighBP in Mississippi is greater than a national average.
  # H0: Avg HighBP in Mississippi (mu_mississippi) <= National Average (mu_national_avg)
  # H1: Avg HighBP in Mississippi (mu_mississippi) > National Average (mu_national_avg) (Upper-tail test)
  
  # Extract HighBloodPressure data for Mississippi.
  Table6_mississippi <- merged_df[merged_df$State=='Mississippi', c(11)] 
  
  # Calculate sample statistics for Mississippi's HighBloodPressure
  xbar_mississippi <- mean(Table6_mississippi, na.rm = TRUE)
  s_mississippi <- sd(Table6_mississippi, na.rm = TRUE)
  n_mississippi <- length(Table6_mississippi) # Number of observations (counties) for Mississippi
  
  # Hypothesized population mean (national average for HighBP, as a percentage).
  # Reference: CDC data, indicating nearly half of adults (implies around 50%, but 48.1 was used).
  mu_national_avg <- 48.1 
  
  alpha <- 0.05 # Significance level
  
  # Calculate the t-statistic for the one-sample t-test.
  t_stat_one_sample <- (xbar_mississippi - mu_national_avg) / (s_mississippi / sqrt(n_mississippi))
  cat("One-Sample T-test for Mississippi HighBP vs National Average:\n")
  cat("Sample Mean (Mississippi HighBP):", round(xbar_mississippi, 3), "\n")
  cat("T-statistic:", round(t_stat_one_sample, 3), "\n")
  
  # Calculate the critical t-value for an upper-tail test.
  t_alpha_one_sample <- qt(1 - alpha, df = n_mississippi - 1)
  cat("Critical t-value (upper tail):", round(t_alpha_one_sample, 3), "\n")
  
  # Calculate the p-value for the upper-tail test.
  pval_one_sample <- pt(t_stat_one_sample, df = n_mississippi - 1, lower.tail = FALSE)
  cat("P-value (upper tail):", round(pval_one_sample, 3), "\n")
  
  # Decision based on t-statistic and p-value.
  if (t_stat_one_sample > t_alpha_one_sample && pval_one_sample < alpha) {
    cat("Decision: Reject H0. Average HighBP in Mississippi is significantly greater than the national average.\n")
  } else {
    cat("Decision: Fail to reject H0. No sufficient evidence that HighBP in Mississippi is greater than national average.\n")
  }
  
  # Plot for one-sample t-test
  tmp_norm_ht1 <- rnorm(100000, mean = 0, sd = 1) # Generate data for standard normal density plot
  dens_norm_ht1 <- density(tmp_norm_ht1)
  plot(dens_norm_ht1, main=paste('One-Sample T-Test (Upper Tail, df=', round(n_mississippi-1), ')', sep=''), 
       xlab='T-value', ylab='Density', type='n', 
       xlim=c(min(t_stat_one_sample, -4, t_alpha_one_sample-1), max(t_stat_one_sample, 4, t_alpha_one_sample+1))) # Adjust xlim for better visualization
  polygon(dens_norm_ht1$x, dens_norm_ht1$y, col='grey') # Fill density curve
  abline(v = t_alpha_one_sample, col = 'red', lwd = 2) # Critical value line
  abline(v = t_stat_one_sample, lty = 2, lwd = 2, col = 'purple') # Test statistic line
  # Shade the rejection region (upper tail)
  x1_shade_ht1 <- min(which(dens_norm_ht1$x >= t_alpha_one_sample))
  x2_shade_ht1 <- length(dens_norm_ht1$x) 
  with(dens_norm_ht1, polygon(x=c(x[c(x1_shade_ht1, x1_shade_ht1:x2_shade_ht1, x2_shade_ht1)]), y= c(0, y[x1_shade_ht1:x2_shade_ht1], 0), col="skyblue"))
  legend("topleft", legend = c("Critical Value", "Test Statistic"), col = c("red", "purple"), lty = c(1, 2), cex=0.8)
  
  # --- Two Sample T-Test ---
  # Objective: Test if the average HighBP in the South region is greater than in the Midwest region.
  # H0 : mu_south <= mu_midwest
  # H1 : mu_south > mu_midwest (Upper-tail test)
  
  # Sample from South region
  pop1_south <- merged_df$HighBloodPressure[merged_df$Region=="South"]
  sample1 <- sample(pop1_south, size = 10, replace = FALSE)
  Xbar1 <- mean(sample1, na.rm=TRUE)
  s1 <- sd(sample1, na.rm=TRUE)
  n1 <- length(sample1)
  
  # Sample from Midwest region
  set.seed(3127109) # Specific seed for this sample
  pop2_midwest <- merged_df$HighBloodPressure[merged_df$Region=="Midwest"]
  sample2 <- sample(pop2_midwest, size = 10, replace = FALSE)
  Xbar2 <- mean(sample2, na.rm=TRUE)
  s2 <- sd(sample2, na.rm=TRUE)
  n2 <- length(sample2)

  cat("\n\nTwo-Sample T-test for South vs Midwest HighBP:\n")
  cat("Sample 1 (South): Mean=", round(Xbar1,3), "SD=", round(s1,3), "n=", n1, "\n")
  cat("Sample 2 (Midwest): Mean=", round(Xbar2,3), "SD=", round(s2,3), "n=", n2, "\n")
  
  # Calculate degrees of freedom using Welch-Satterthwaite equation for unequal variances.
  df_two_sample <- (((s1^2/n1) + (s2^2/n2))^2) / ((((s1^2/n1)^2)/(n1-1)) + ((s2^2/n2)^2/(n2-1)))
  cat("Degrees of Freedom (Welch-Satterthwaite):", round(df_two_sample, 3), "\n")
  
  # Calculate critical t-value for an upper-tail test.
  crl_val_two_sample <- qt(1 - alpha, df = df_two_sample)
  cat("Critical t-value (upper tail):", round(crl_val_two_sample, 3), "\n")
  
  # Calculate the t-statistic for the two-sample t-test (assuming unequal variances).
  t_stat_two_sample <- (Xbar1 - Xbar2) / sqrt((s1^2/n1) + (s2^2/n2))
  cat("T-statistic:", round(t_stat_two_sample, 2), "\n")
  
  # Calculate the p-value for the upper-tail test.
  pval_two_sample <- pt(t_stat_two_sample, df = df_two_sample, lower.tail = FALSE)
  cat("P-value (upper tail):", round(pval_two_sample,3), "\n")
  
  # Decision based on t-statistic and p-value.
  if (t_stat_two_sample > crl_val_two_sample && pval_two_sample < alpha) {
    cat("Decision: Reject H0. Average HighBP in South is significantly greater than Midwest.\n")
  } else {
    cat("Decision: Fail to reject H0. No sufficient evidence that HighBP in South is greater than Midwest.\n")
  }

  # Plot for two-sample t-test
  plot(dens_norm_ht1, main=paste('Two-Sample T-Test (Upper Tail, df=', round(df_two_sample),')', sep=''), 
       xlab='T-value', ylab='Density', type='n', 
       xlim=c(min(t_stat_two_sample, -4, crl_val_two_sample-1), max(t_stat_two_sample, 6, crl_val_two_sample+1)))
  polygon(dens_norm_ht1$x, dens_norm_ht1$y, col='grey')
  abline(v = crl_val_two_sample, col = 'red', lwd = 2)
  abline(v = t_stat_two_sample, lty = 2, lwd = 2, col = 'purple')
  # Shade the rejection region
  x1_shade_ht2 <- min(which(dens_norm_ht1$x >= crl_val_two_sample))
  x2_shade_ht2 <- length(dens_norm_ht1$x)
  with(dens_norm_ht1, polygon(x=c(x[c(x1_shade_ht2, x1_shade_ht2:x2_shade_ht2, x2_shade_ht2)]), y= c(0, y[x1_shade_ht2:x2_shade_ht2], 0), col="skyblue"))
  legend("topright", legend = c("Critical Value", "Test Statistic"), col = c("red", "purple"), lty = c(1, 2), cex=0.8)
}