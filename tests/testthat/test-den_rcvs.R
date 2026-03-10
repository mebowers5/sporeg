test_that("returns correct types", {
  receiver_density <- den_rcvs(res[[1]])

  expect_s3_class(receiver_density, "data.frame")
  expect_type(receiver_density, "list")
  expect_named(
    receiver_density,
    c("dn_min", "dn_mean", "dn_max", "c_min", "c_mean", "c_max")
  )
  expect_s3_class(receiver_density$dn_min, "units")
  expect_s3_class(receiver_density$dn_mean, "units")
  expect_s3_class(receiver_density$dn_max, "units")
  expect_equal(
    attr(receiver_density$dn_mean, "units")$denominator,
    c("km", "km")
  )
})
