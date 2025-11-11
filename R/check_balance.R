#' Testing Covariate Balance
#'
#' @param data Data frame containing the variables
#' @param treatment Name of treatment/exposure variable (must be binary)
#' @param num Vector of numeric variable names (e.g., c("age", "income"))
#' @param cat Vector of categorical variable names (e.g., c("sex", "race"))
#' @param round Number of decimal places for rounding numeric outputs
#'              (default: NULL for no rounding)
#' @returns A list
#' @export
#'
#' @examples
#' check_balance(data = lalonde,
#'               treatment = "treat",
#'               num = c("age", "educ"),
#'               cat = c("race", "married"),
#'               round = 3)
check_balance <- function(data, treatment,
                          num = NULL,
                          cat = NULL,
                          round = NULL) {
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
  # Convert num to vector if it's a single character (and not NULL)
  if (!is.null(num) && is.character(num) && length(num) == 1) {
    num <- c(num)
  }
  # Convert cat to vector if it's a single character (and not NULL)
  if (!is.null(cat) && is.character(cat) && length(cat) == 1) {
    cat <- c(cat)
  }
  # Validate that num and cat are vectors (if provided)
  if (!is.null(num) && !is.vector(num)) {
    stop("'num' must be a vector or NULL, found: ", class(num))
  }
  if (!is.null(cat) && !is.vector(cat)) {
    stop("'cat' must be a vector or NULL, found: ", class(cat))
  }

  # Calculate treatment distribution
  treat_counts <- table(data[[treatment]])
  treat_distribution <- prop.table(treat_counts)

  if (!is.null(num)) {
    res_num <- data.frame()
    for (i in 1:length(num)) {
      result <- broom::tidy(t.test(as.formula(paste(num[i], "~", treatment)), data = data))
      result$variable <- num[i]
      res_num <- rbind(res_num, result)
    }
    if (!is.null(round)) {
      cols_num <- c("estimate", "estimate1", "estimate2",
                    "statistic", "parameter", "conf.low", "conf.high")
      res_num[cols_num] <- lapply(res_num[cols_num], function(x) round(x, round))
      res_num$p.value <- ifelse(res_num$p.value < 10^(-round),
                                paste("<", 10^(-round)),
                                sprintf(paste0("%.", round, "f"), res_num$p.value))
    }
    colnames(res_num)[1:3] <- c("mean_difference", "mean_control", "mean_treated")
    colnames(res_num)[6:8] <- c("df", "conf_low", "conf_high")
    reorder_num <- c("variable", "statistic", "p.value", "df",
                     "mean_control", "mean_treated", "mean_difference",
                     "conf_low", "conf_high", "method")
    res_num <- res_num[, reorder_num]
  } else {
    res_num <- data.frame()
  }

  if (!is.null(cat)) {
    res_cat <- data.frame(
      variable = character(),
      statistic = numeric(),
      p.value = numeric(),
      df = numeric(),
      mean_control = numeric(),
      mean_treated = numeric(),
      mean_difference = numeric(),
      conf_low = numeric(),
      conf_high = numeric(),
      method = character())
    for (i in 1:length(cat)) {
      result <- data.frame(
        variable = cat[i],
        statistic = NA,
        p.value = NA,
        df = NA,
        mean_control = NA,
        mean_treated = NA,
        mean_difference = NA,
        conf_low = NA,
        conf_high = NA,
        method = NA)
      tbl <- table(data[[treatment]], data[[cat[i]]])

      chi_test <- tryCatch({ # Try chi-square test first
        chisq.test(tbl)
      }, error = function(e) {
        return(NULL)  # Return NULL if chi-square fails
      })
      if (!is.null(chi_test)) { # Chi-square worked, check expected counts
        expected <- chi_test$expected
        if (any(expected < 5)) {
          # Small expected counts - use Fisher's
          result_fisher <- broom::tidy(fisher.test(tbl))
          result$p.value <- result_fisher$p.value
          result$method <- result_fisher$method
        } else {
          # Expected counts OK - use chi-square
          result_chisq <- broom::tidy(chi_test)
          result$statistic <- result_chisq$statistic
          result$p.value <- result_chisq$p.value
          result$df <- result_chisq$parameter
          result$method <- result_chisq$method
        }
      } else {
        # Chi-square failed entirely - use Fisher's
        result_fisher <- broom::tidy(fisher.test(tbl))
        result$p.value <- result_fisher$p.value
        result$method <- result_fisher$method
      }
      res_cat <- dplyr::bind_rows(res_cat, result)
    }
    if (!is.null(round)) {
      res_cat$statistic <- ifelse(is.na(res_cat$statistic),
                                  NA,
                                  round(res_cat$statistic, round))
      res_cat$p.value <- ifelse(res_cat$p.value < 10^(-round),
                                paste("<", 10^(-round)),
                                sprintf(paste0("%.", round, "f"), res_cat$p.value))
    }
  } else {
    res_cat <- data.frame()
  }

  res_balance <- dplyr::bind_rows(res_num, res_cat)

  return(list(
    treatment_counts = treat_counts,
    treatment_distribution = treat_distribution,
    balance_results = res_balance
  ))
}
