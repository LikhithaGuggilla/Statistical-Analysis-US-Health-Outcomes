**Objective:**
A statistical evaluation and modeling solution using R aimed at identifying, forecasting prevalent health outcome and discovering it's association with preventive measures, health-risk behaviors among American population across 4 regions, 50 states and 3142 counties.

**Data:**
Each row represents '% of people' having a health issue, their behavioral risk indicators and engagement in recommended health preventive measures by county, state and region.
[There are 13 health outcomes, 12 preventive measures & 5 health-risk behaviours]

**Source:**
CDC PLACES data (https://www.cdc.gov/places/index.html)

**Data Preprocessing:**
1. Raw CDC health data has unrequired features while required features are in unpivoted manner ~2,30,000 x 40. Dealt this with elimination, data restructuring, pivoting necessary columns/features reducing the dataset size to ~3200 rows x 30 contributing to clean analysis and model development.

**EDA & Hypothesis Testing:**
1. Extracted insights on most prevalent health issue (High BP) & predominant areas in USA using advanced aggregations & bar, scatter, box plots. Validated these observations with one-sample, two-sample hypothesis tests (z-tests) formulating decision rules and statistically confirming the observations using p-value at 95% confidence.
2. Performed correlation and regression analysis revealing the risk behaviors & preventive measures strongly associated with High BP prevalence (especially in predominant region).

**Statistical Modeling:**
1. Built and compared 4 full & reduced regression models forecasting High BP proportions, while improving model performance by 15% identifying correlated risk behaviors with stepwise iterations and eliminating multi-collinearity with VIF.
2. Performed model residual analysis (using box plots) revealing model performance discrepancies by region, states, counties delivering region-specific under-addressed healthcare needs.

**Evaluation Metric:**
AIC, Adjusted R-squared, MAE, F-statistic


**Future Scope:**
This foundation can be extended into diagnostic analytics by incorporating causal inference techniques, such as Randomized Controlled Trials (RCTs) or quasi-experimental designs, to move beyond correlation and establish causal drivers of High BP, ultimately enabling targeted, evidence-based health interventions.

Skills: R (dplyr, ggplot2, statsmodel), z-test, Stepwise Regression, Correlation