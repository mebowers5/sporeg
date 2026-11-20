test_that("returns sf object in input CRS", {
  test_data <- fake_track()
  crawl <- structure(list(), class = "sporeg_crawl")

  out <- impute_track(
    model = crawl,
    data = test_data,
    timeStep = 0.5,
    attempts = 5,
    retryFits = 1,
    retryParallel = FALSE
  )

  expect_s3_class(out, "sf")
  expect_true(all(c("ID", "uid") %in% names(out)))
  expect_equal(sf::st_crs(out), sf::st_crs(test_data))
  expect_false(any(sf::st_is_empty(out)))
})

test_that("overrides arguments via dots", {
  test_data <- fake_track()
  crawl <- structure(list(), class = "sporeg_crawl")

  out_coarse <- impute_track(
    model = crawl,
    data = test_data,
    timeStep = 1,
    attempts = 5,
    retryFits = 1,
    retryParallel = FALSE
  )

  out_fine <- impute_track(
    model = crawl,
    data = test_data,
    timeStep = 0.25,
    attempts = 5,
    retryFits = 1,
    retryParallel = FALSE
  )

  expect_gt(nrow(out_fine), nrow(out_coarse))
})
