test_that("returns correct types", {
  tracks <- simul_trks(
    anims = 2,
    study_site = fo_study_site,
    theta = c(0, 1.74),
    vmin = 0.98,
    vmax = 1.58,
    rel_site = fo_rel_site,
    crs = 3857,
    n_days = 30,
    initHeading = 0
  ) |>
    expect_output("Simulating tracks...\n|==")

  expect_s3_class(
    tracks,
    c("grouped_df", "tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )

  expect_named(tracks, c("ID", "data"))

  expect_type(tracks$data, "list")
})

test_that("data column types are correct", {
  tracks <- simul_trks(
    anims = 2,
    study_site = fo_study_site,
    theta = c(0, 1.74),
    vmin = 0.98,
    vmax = 1.58,
    rel_site = fo_rel_site,
    crs = 3857,
    n_days = 30,
    initHeading = 0
  )

  expect_s3_class(
    tracks$data[[1]],
    c("sf", "tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
  expect_named(
    tracks$data[[1]],
    c(
      "uid",
      "geom",
      "POINT_Y",
      "POINT_X",
      "time",
      "start_x",
      "start_y",
      "end_x",
      "end_y",
      "x"
    )
  )
})
