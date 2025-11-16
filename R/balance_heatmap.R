#' Balance Heatmap for Numeric and Categorical Covariates
#'
#' @description
#' Creates a heatmap visualizing covariates imbalance between treatment groups
#' based on standardized mean differences. Numeric variables use
#' mean-based SMD, while categorical variables use SMD for proportions.
#' Covariates with SMD greater than the midpoint (default 0.1) are highlighted in red.
#'
#' @param data Data frame containing the variables.
#' @param treatment Name of the binary treatment variable.
#' @param num Vector of numeric variable names (e.g., c("age", "educ", "re74"))
#' @param cat Vector of categorical variable names (e.g., c("race", "married", "nodegree"))
#' @param title Optional plot title.
#'
#' @return A ggplot heatmap showing standardized mean differences.
#' @export
#'
#' @examples
#' balance_heatmap(data = lalonde,
#'                      treatment = "treat",
#'                      num = c("age", "educ", "re74"),
#'                      cat = c("race", "married", "nodegree"),
#'                      title = "Covariate Balance Heatmap"
#' )
balance_heatmap <- function(data, treatment,
                                 num = NULL, cat = NULL,
                                 title = "Balance Heatmap") {
  # Check treatment variable exists
  if (!treatment %in% names(data)) {
    stop("Treatment variable '", treatment, "' not found in data")
  }
  # Check treatment is binary (0/1 or 1/2)
  treat_values <- unique(data[[treatment]])
  if (length(treat_values) != 2) {
    stop("Treatment variable must have exactly 2 levels, found: ", length(treat_values))
  }
  # Check that at least one of num or cat is provided
  if (is.null(num) && is.null(cat)) {
    stop("At least one of 'num' or 'cat' must be provided (cannot both be NULL)")
  }

  # convert single variable to vector
  if (!is.null(num) && is.character(num) && length(num) == 1) num <- c(num)
  if (!is.null(cat) && is.character(cat) && length(cat) == 1) cat <- c(cat)

  compute_smd <- function(var, treat) {

    # calculate standardized mean difference when variate is numeric
    if (is.numeric(var)) {
      m1 <- mean(var[treat == treat_values[1]], na.rm = TRUE)
      m2 <- mean(var[treat == treat_values[2]], na.rm = TRUE)
      sd1 <- sd(var[treat == treat_values[1]], na.rm = TRUE)
      sd2 <- sd(var[treat == treat_values[2]], na.rm = TRUE)

      smd <- abs(m1 - m2) / sqrt((sd1^2 + sd2^2) / 2)
      return(smd)
    }

    # calculate standardized mean difference when variate is categorical
    else if (is.factor(var) || is.character(var)) {
      tbl <- table(treat, var)

      smd <- max(unlist(lapply(colnames(tbl), function(level) {
        p1 <- tbl[1, level] / sum(tbl[1, ])
        p2 <- tbl[2, level] / sum(tbl[2, ])

        abs(p1 - p2) / sqrt((p1*(1 - p1) + p2*(1 - p2)) / 2)})))
      return(smd)
    }
  }

  all_vars <- c(num, cat)
  smd_vals <- as.vector(sapply(all_vars, function(var) {
    compute_smd(data[[var]], data[[treatment]])
  }))
  types <- ifelse(all_vars %in% num, "numeric", "categorical")

  results <- data.frame(
    variable = all_vars,
    smd = smd_vals,
    type = types
  )
  # switch order of rows by descending SMD
  results <- results[order(results$smd, decreasing = FALSE), ]
  results$variable <- factor(results$variable, levels = results$variable)

  print(results)

  # plot the heatmap
  library(ggplot2)

  ggplot(results, aes(x = "SMD", y = variable, fill = smd)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "white",
      mid = "skyblue",
      high = "darkblue",
      midpoint = 0.1,
      name = "Standardized Mean Difference"
    ) + geom_text(aes(
      label = ifelse(smd > 0.1, paste0(round(smd,4), " > 0.1"), round(smd,4)),
      color = smd > 0.1
    ), size = 4) +
    scale_color_manual(values = c("FALSE" = "white", "TRUE" = "red"), guide = "none") +
    theme_minimal(base_size = 12) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) +
    ylab("Covariate") +
    ggtitle(title)
}
