test_that("receivers are randomized", {
  out <- randomize_receivers(fo_study_site, 100, 500, FALSE)

  expect_s3_class(out, c("sfc", "sfc_POLYGON"))
  expect_length(out, 100)
})

test_that("receivers are randomized", {
  out <- randomize_receivers(fo_study_site, 100, 500, FALSE)

  expect_s3_class(out, c("sfc", "sfc_POLYGON"))
  expect_length(out, 100)
})

test_that("map is created when view = T", {
  printed <- FALSE

  testthat::local_mocked_bindings(
    html_print = function(html, ...) {
      printed <<- TRUE
      invisible(NULL)
    },
    .package = "htmltools"
  )

  out <- randomize_receivers(fo_study_site, 100, 500, T)

  expect_true(printed)
})

test_that("map is not created when view = F", {
  printed <- FALSE

  testthat::local_mocked_bindings(
    html_print = function(html, ...) {
      printed <<- TRUE
      invisible(NULL)
    },
    .package = "htmltools"
  )

  out <- randomize_receivers(fo_study_site, 100, 500, F)

  expect_false(printed)
})
