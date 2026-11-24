test_that("returns correct types", {
  tracks <- simulate_tracks(
    anims = 2,
    start_area = fo_rel_site,
    study_area = fo_study_site,
    n_days = 30,
    # Arguments passed to `glatos::crw_in_polygon`
    initHead = 0,
    theta = c(0, 1.74),
    vmin = 0.98,
    vmax = 1.58
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
  tracks <- simulate_tracks(
    anims = 2,
    start_area = fo_rel_site,
    study_area = fo_study_site,
    n_days = 30,
    initHead = 0,
    theta = c(0, 1.74),
    vmin = 0.98,
    vmax = 1.58
  )

  expect_s3_class(
    tracks$data[[1]],
    c("tbl_df", "tbl", "data.frame"),
    exact = TRUE
  )
  expect_named(
    tracks$data[[1]],
    c(
      "uid",
      "start_x",
      "start_y",
      "end_x",
      "end_y",
      "geometry"
    )
  )
})
