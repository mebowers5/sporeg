test_that("fits and returns sf in input CRS", {
  test_data <- fake_track()

  out <- impute_track(
    model = animotum_ssm(model = "rw", time.step = 12),
    data = test_data
  )

  expect_s3_class(out, "sf")
  expect_true(all(c("ID", "uid") %in% names(out)))
  expect_equal(sf::st_crs(out), sf::st_crs(test_data))
  expect_gt(nrow(out), nrow(test_data))
})

test_that("dots overrides arguments supplied in animotum_ssm()", {
  test_data <- fake_track()

  out_24h <- impute_track(
    model = animotum_ssm(model = "rw", time.step = 24),
    data = test_data
  )

  out_6h <- impute_track(
    model = animotum_ssm(model = "rw", time.step = 24),
    data = test_data,
    time.step = 6
  )

  expect_gt(nrow(out_6h), nrow(out_24h))
})
