test_that("works with custom model dots", {
  test_data <- fake_track()

  custom_model <- function(data, nudge = 0, ...) {
    geom <- sf::st_geometry(data) + c(nudge, nudge)
    sf::st_sf(
      ID = data$ID,
      uid = data$uid,
      geometry = geom,
      crs = sf::st_crs(data)
    )
  }

  out <- impute_track(model = custom_model, data = test_data, nudge = 10)

  expect_s3_class(out, "sf")
  expect_true(all(c("ID", "uid") %in% names(out)))
  expect_equal(sf::st_crs(out), sf::st_crs(test_data))
  expect_equal(
    sf::st_coordinates(out)[, 1],
    sf::st_coordinates(test_data)[, 1] + 10
  )
})

test_that("errors on unknown S3 model classes", {
  test_data <- fake_track()
  bad_model <- structure(list(), class = "sporeg_nonexistent")

  expect_error(impute_track(model = bad_model, data = test_data))
})
