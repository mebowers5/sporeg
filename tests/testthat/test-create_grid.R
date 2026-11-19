test_that("returns correct types", {
  create_grid(study_site, 1000, "centers") |>
    expect_s3_class(c("sf", "data.frame"), exact = TRUE)
})

test_that("returns polygons", {
  result <- create_grid(study_site, 1000, "polygons")

  expect_named(result, c("Id", "gid", "geometry"))
  expect_s3_class(result$geometry, c("sfc_MULTIPOLYGON", "sfc"), exact = TRUE)
})

test_that("returns centers", {
  result <- create_grid(study_site, 1000, "centers")

  expect_named(result, c("gid", "geometry"))
  expect_s3_class(result$geometry, c("sfc_POINT", "sfc"), exact = TRUE)
})

test_that("errors with incorrect 'what' argument", {
  create_grid(study_site, 1000, "gibberish") |>
    expect_error("'arg' should be one of \"centers\", \"polygons\"")
})

test_that("errors if not in a projected coordinate system", {
  study_site |>
    sf::st_transform(4326) |>
    create_grid(1000, "centers") |>
    expect_error("Study area is not in a projected coordinate system.")
})
