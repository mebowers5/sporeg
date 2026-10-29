# test_that("returns correct types", {
#   result <- gps_fld(resolutions[[3]])

#   expect_s3_class(result, "data.frame", exact = TRUE)
#   expect_named(result, c("n", "tot", "x", "perc"))
#   expect_s3_class(result$x, "sfc")
# })
