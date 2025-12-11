test_that("plot_balance function works", {
  test_data <- data.frame(
    treat = c(0, 1, 0, 1),
    age = c(21, 36, 45, 58),
    gender = c("F", "M", "F", "M")
  )

  result <- plot_balance(data = test_data, treatment = "treat", num = "age", cat = "gender")

  expect_s3_class(result$numeric, "ggplot")
  expect_s3_class(result$categorical, "ggplot")
})
