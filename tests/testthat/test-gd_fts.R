test_that("returns correct types", {
  gd_fts(res[[2]]) |>
    expect_s3_class(c("sf", "data.frame"), exact = TRUE) |>
    expect_type("list") |>
    expect_named(c(
      "max_dpth",
      "min_dpth",
      "avg_depth",
      "max_d",
      "min_d",
      "avg_d",
      "max_ct",
      "min_ct",
      "avg_ct",
      "n_abs",
      "tot",
      "perc_abs",
      "x"
    ))
})

test_that("retains projection", {
  res_projection <- sf::st_crs(res[[2]])

  gd_projection <- gd_fts(res[[2]]) |>
    sf::st_crs()

  expect_equal(gd_projection, res_projection)
})
