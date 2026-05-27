test_that("returns correct types", {
  result <- gps_chr(res[[3]])

  expect_s3_class(result, c("sf", "data.frame"), exact = TRUE)
  expect_named(
    result,
    c("max_depth", "min_depth", "avg_depth", "max_d", "min_d", "avg_d", "x")
  )
})

test_that("retains projection", {
  res_projection <- sf::st_crs(res[[2]])

  gps_chr_projection <- gps_chr(res[[2]]) |>
    sf::st_crs()

  expect_equal(gps_chr_projection, res_projection)
})
