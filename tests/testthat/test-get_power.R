# Use powr wrapper function on example results
# Grab 100km resolution
res <- lapply(sporeg::results, `[[`, 1) |>
  dplyr::bind_rows() |>
  # Calculate standard deviation by group
  dplyr::group_by(gid) |>
  dplyr::summarise(sd = sd(dif)) |>
  dplyr::ungroup()
anims <- 30

test_that("returns correct classes", {
  powr_result <- get_power(
    res,
    sig_level = 0.95,
    power = 0.8,
    delta = anims * 0.01
  )

  expect_s3_class(powr_result, "power.htest")
  expect_named(
    powr_result,
    c("n", "delta", "sd", "sig.level", "power", "alternative", "note", "method")
  )
})

test_that("handles zero variance", {
  powr_result_zerovar <- res |>
    dplyr::bind_rows(
      data.frame(sd = rep(0, 50))
    ) |>
    get_power(sig_level = 0.95, power = 0.8, delta = anims * 0.01)

  powr_result <- get_power(
    res,
    sig_level = 0.95,
    power = 0.8,
    delta = anims * 0.01
  )

  expect_identical(powr_result_zerovar, powr_result)
})
