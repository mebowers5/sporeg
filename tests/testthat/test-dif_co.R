test_that("returns correct types", {
  diff_cutoff <- dif_co(res[[1]], depth_limit = 300)

  diff_cutoff |>
    expect_type("list") |>
    expect_s3_class("data.frame") |>
    expect_named(c("tot", "n", "x", "perc"))
})

test_that("retains geometry column", {
  diff_cutoff <- dif_co(res[[1]], depth_limit = 30) |>
    _$x |>
    expect_s3_class(c("sfc_MULTIPOLYGON", "sfc"), exact = TRUE)
})
