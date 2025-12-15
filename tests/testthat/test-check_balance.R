test_that("treatment counts have length 2", {
  bal <- check_balance(data = lalonde,
                       treatment = "treat",
                       ref = 0,
                       num = c("age", "educ"),
                       cat = c("race", "married"),
                       round = 3)
  expect_equal(length(bal$treatment_counts), 2)
})

test_that("treatment distribution is consistent with the counts", {
  bal <- check_balance(data = lalonde,
                       treatment = "treat",
                       ref = 0,
                       num = c("age", "educ"),
                       cat = c("race", "married"),
                       round = 3)
  expect_equal(bal$treatment_distribution,
               bal$treatment_counts/sum(bal$treatment_counts))
})

test_that("balance result table has the correct dimensions", {
  num <- c("age", "educ")
  cat <- c("race", "married")
  bal <- check_balance(data = lalonde,
                       treatment = "treat",
                       ref = 0,
                       num = num,
                       cat = cat,
                       round = 3)
  expect_equal(dim(bal$balance_results),
               c(length(num)+length(cat), 10))
})
