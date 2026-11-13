test_that("returns correct types", {
  receiver_info(resolutions[[1]]) |>
    expect_s3_class("data.frame") |>
    expect_named(
      c(
        "total",
        "depth_200_800",
        "depth_200_500",
        "min_depth",
        "avg_depth",
        "max_depth",
        "max_distance",
        "min_distance",
        "avg_distance"
      )
    )
})
