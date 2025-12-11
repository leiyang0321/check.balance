test_that("balance_heatmap works", {
  result <- balance_heatmap(
    data      = lalonde,
    treatment = "treat",
    num       = c("age", "educ", "re74"),
    cat       = c("race", "married", "nodegree"),
    title     = "Covariate Balance Heatmap"
  )
  expect_s3_class(result, "ggplot")})
