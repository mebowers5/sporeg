test_that("returns correct types", {
  fit <- get_or(fit2.100km)

  expect_s3_class(fit, "data.frame")
  expect_named(fit, c("Effect", "Odds_Ratio", "low_CI_95", "hi_CI_95"))
  expect_equal(nrow(fit), 4)
  expect_equal(fit$Effect, summary(fit2.100km)$coefficients |> row.names())
  expect_type(fit$Odds_Ratio, "double")
  expect_type(fit$low_CI_95, "double")
  expect_type(fit$hi_CI_95, "double")
})
