test_that("returns correct types", {
  diff_cutoff <- dif_co(res[[1]], depth_limit = 300)

  diff_cutoff |>
    expect_type("list") |>
    expect_s3_class(c("data.frame", "sf"), exact = TRUE) |>
    expect_named(c("tot", "n", "x", "perc"))
})
