#' Plot Covariate Balance
#'
#' @description
#' Creates side-by-side plots for numeric and categorical covariates after
#' running check_balance.
#' Numeric covariates can be displayed as histograms or boxplots.
#' Categorical covariates are displayed as bar plots
#'
#' @param data Data frame used in check_balance.
#' @param treatment Treatment variable used in check_balance (must be binary).
#' @param num Vector of numeric variable names (e.g., c("age", "educ"))
#' @param cat Vector of categorical variable names (e.g., c("race", "married"))
#' @param numeric_plot Either "histogram" or "boxplot" for numeric covariates
#' (default "histogram).
#'
#' @returns A list of ggplot objects: $numeric, $categorical
#' @export
#'
#' @examples
#' # After running check_balance
#' plot_balance(data = lalonde,
#'              treatment = "treat",
#'              num = c("age", "educ"),
#'              cat = c("race", "married"),
#'              numeric_plot = "histogram")
#'
plot_balance <- function(data, treatment, num = NULL, cat = NULL,
                         numeric_plot = "histogram"){
  library(tidyverse)
  library(ggplot2)

  # Convert treatment to factor
  data[[treatment]] <- factor(data[[treatment]])

  # Use provided vectors
  if(is.null(num)) {
    num <- names(data)[sapply(data, is.numeric) & names(data) != treatment]
  }
  if(is.null(cat)) {
    cat <- names(data)[sapply(data, function(x) is.factor(x) ||
                                is.character(x)) & names(data) != treatment]
  }

  # Numeric Plots
  p_numeric <- NULL
  if(length(num) > 0){
    num_long <- data %>%
      select(all_of(c(treatment, num))) %>%
      pivot_longer(cols = -all_of(treatment), names_to = "Variable",
                   values_to = "Value")

    if(numeric_plot == "histogram"){
      p_numeric <- ggplot(num_long, aes(x = Value, fill = !!sym(treatment))) +
        geom_histogram(alpha = 0.6, position = "identity",
                       bins = 30, color = "black") +
        facet_wrap(~ Variable, scales = "free") +
        scale_fill_manual(values = setNames(c("steelblue", "orange"), levels(data[[treatment]]))) +
        theme_minimal() +
        labs(color = "Treatment", title = "Histograms for Numeric Covariates", x = "Value",
             y = "Count", fill = treatment)
    } else if(numeric_plot == "boxplot"){
      p_numeric <- ggplot(num_long, aes(x = !!sym(treatment), y = Value,
                                        fill = !!sym(treatment))) +
        geom_boxplot(alpha = 0.6, outlier.shape = NA) +
        geom_jitter(width = 0.2, alpha = 0.6, color = "black") +
        facet_wrap(~ Variable, scales = "free_y") +
        scale_color_manual(values = setNames(c("steelblue", "orange"), levels(data[[treatment]]))) +
        theme_minimal() +
        labs(title = "Boxplots for Numeric Covariates", x = "Group",
             y = "Value", fill = treatment)
    } else {
      stop('numeric_plot must be either "histogram" or "boxplot"')
    }
  }

  # Categorical Plots
  p_categorical <- NULL
  if(length(cat) > 0){
    cat_factor <- data %>% select(all_of(c(treatment, cat))) %>%
      mutate(across(-all_of(treatment), as.character))

    cat_long <- cat_factor %>%
      pivot_longer(cols = -all_of(treatment), names_to = "Variable",
                   values_to = "Level")

    p_categorical <- ggplot(cat_long, aes(x = Level, fill = !!sym(treatment))) +
      geom_bar(position = "dodge") +
      facet_wrap(~ Variable, scales = "free") +
      scale_color_manual(values = setNames(c("steelblue", "orange"), levels(data[[treatment]]))) +
      theme_minimal() +
      labs(title = "Bar Plots for Categorical Covariates", x = "Level",
           y = "Count", fill = treatment)
  }

  return(list(
    numeric = p_numeric,
    categorical = p_categorical
  ))
}
