test_that("returns correct types", {
  rcv_chr(res[[1]]) |>
    expect_s3_class("data.frame") |>
    expect_named(
      c(
        "tot",
        "depth_200_800",
        "depth_200_500",
        "min_depth",
        "avg_depth",
        "max_depth",
        "max_d",
        "min_d",
        "avg_d"
      )
    )
})
