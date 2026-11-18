test_that("returns correct types", {
  rec_dens <- receiver_density(resolutions[[1]])

  expect_s3_class(rec_dens, "data.frame")
  expect_type(rec_dens, "list")
  expect_named(
    rec_dens,
    c(
      "density_min",
      "density_mean",
      "density_max",
      "count_min",
      "count_mean",
      "count_max"
    )
  )
  expect_s3_class(rec_dens$density_min, "units")
  expect_s3_class(rec_dens$density_mean, "units")
  expect_s3_class(rec_dens$density_max, "units")
  expect_equal(
    attr(rec_dens$density_mean, "units")$denominator,
    c("km", "km")
  )
})
