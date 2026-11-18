test_that("returns correct types", {
  result <- gaps_info(resolutions[[3]])

  expect_s3_class(result, c("sf", "data.frame"), exact = TRUE)
  expect_named(
    result,
    c("max_depth", "min_depth", "avg_depth", "max_d", "min_d", "avg_d", "x")
  )
})

test_that("retains projection", {
  res_projection <- sf::st_crs(resolutions[[2]])

  gps_chr_projection <- gaps_info(resolutions[[2]]) |>
    sf::st_crs()

  expect_equal(gps_chr_projection, res_projection)
})
