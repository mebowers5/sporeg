test_that("returns correct types", {
  diff_cutoff <- change_cutoff(resolutions[[1]], depth_limit = 300)

  diff_cutoff |>
    expect_type("list") |>
    expect_s3_class("data.frame") |>
    expect_named(c("tot", "n", "perc"))
})
