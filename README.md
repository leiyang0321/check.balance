# check_balance

This R package includes a function (`check_balance`) that returns a descriptive table summarizing the descriptive statistics for two groups, allowing users to conduct group comparisons when needed.

### Purpose

The goal of this function is to summarize and compare descriptive statistics between two groups, allowing users to check baseline equivalence, identify potential confounders, and detect outliers or unexpected distributions. It is designed to support data-driven decision-making and enhance transparency in comparative analyses.

### Packege Functions Description

This package provides the following functions for covariate balance assessment:

#### Primary Function:
**`check_balance`** -  Outputs a descriptive table summarizing the mean of each group, the difference in means between the groups, the test statistic from either a Pearson's chi-squared test or Fisher’s exact test, the corresponding p-value, and the confidence interval. The function is designed to assess whether there are statistically significant differences in group means.

#### Visualization Functions:
1. **`balance_heatmap`** - Creates a heatmap visualizing covariates imbalance between treatment groups based on standardized mean differences. Numeric variables use
mean-based SMD, while categorical variables use SMD for proportions. Covariates with SMD greater than the midpoint (default 0.1) are highlighted in red.

2. **`plot_balance`** - Creates side-by-side plots for numeric and categorical covariates after
running check_balance. Numeric covariates can be displayed as histograms or boxplots. Categorical covariates are displayed as bar plots

### R Installation Instructions

```r
install.packages("devtools") 
library(devtools)
devtools::install_github("leiyang0321/check.balance")
library(check.balance)
```

### Example

For instance, 
- **`check_balance`** identifies differences in predictor means between two groups and computes corresponding test statistics and p-values using Pearson's chi-squared test or Fisher's exact test.
- **`balance_heatmap`** detects imbalance patterns between treatment groups through visual heatmap representation.
- **`plot_balance`** displays distribution patterns for both numeric and categorical covariates to support visual balance assessment. 

```r
check_balance(data = lalonde,
              treatment = "treat",
              ref=0,
              num = c("age", "educ"),
              cat = c("race", "married"),
              round = 3)

balance_heatmap(data = lalonde,
                treatment = "treat",
                num = c("age", "educ", "re74"),
                cat = c("race", "married", "nodegree"),
                title = "Covariate Balance Heatmap")
              
plot_balance(data = lalonde,
             treatment = "treat",
             num = c("age", "educ"),
             cat = c("race", "married"),
             numeric_plot = "histogram")
```

