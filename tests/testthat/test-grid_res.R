test_that("returns correct types", {
  grid_res(1000, site_depth, 4269, "centers") |>
    expect_s3_class(c("sf", "data.frame"), exact = TRUE)
})

test_that("returns correct input CRS", {
  result <- grid_res(1000, site_depth, 4269, "centers")
  expect_equal(sf::st_crs(result)$epsg, 4269)

  result <- grid_res(1000, site_depth, 4326, "centers")
  expect_equal(sf::st_crs(result)$epsg, 4326)
})

test_that("returns polygons", {
  result <- grid_res(1000, site_depth, 4269, "polygons")

  expect_named(result, c("Id", "gid", "geometry"))
  expect_s3_class(result$geometry, c("sfc_MULTIPOLYGON", "sfc"), exact = TRUE)
})

test_that("returns centers", {
  result <- grid_res(1000, site_depth, 4269, "centers")

  expect_named(result, c("gid", "geometry"))
  expect_s3_class(result$geometry, c("sfc_POINT", "sfc"), exact = TRUE)
})

test_that("errors with incorrect 'what' argument", {
  grid_res(1000, site_depth, 4269, "gibberish") |>
    expect_error("`what` must be one of \"polygons\" or \"centers\"")
})

test_that("errors if not in a projected coordinate system", {
  grid_res(1000, sf::st_transform(site_depth, 4326), 4269, "centers") |>
    expect_error("Study site is not in a projected coordinate system.")
})
