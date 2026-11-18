test_that("returns correct types", {
  result <- gaps_filled(resolutions[[3]])

  expect_s3_class(result, c("sf", "data.frame"), exact = TRUE)
  expect_named(result, c("n", "tot", "perc", "x"))
  expect_s3_class(result$x, "sfc")
})

test_that("calculates percent good fits", {
  data.frame(
    p_a = 0,
    g_fit = c(rep(1, 100), rep(0, 50))
  ) |>
    gaps_filled() |>
    expect_equal(
      data.frame(n = 100, tot = 150, perc = (2 / 3) * 100)
    )
})
