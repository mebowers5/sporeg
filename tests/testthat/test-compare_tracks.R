tracks <- simulate_tracks(
  anims = 2,
  study_area = fo_study_site,
  theta = c(0, 1.74),
  vmin = 0.98,
  vmax = 1.58,
  rel_site = fo_rel_site,
  crs = 3857,
  n_days = 30,
  initial_heading = 0
)

hotspot_grid <- create_grid(
  study_area = fo_study_site,
  resolution = 50,
  crs = 3857,
  what = 'polygons'
)

visgraph <- pathroutr::prt_visgraph(fo_land_barrier)

test_that("handles one hotspot grid", {
  out <- compare_tracks(
    tracks,
    fo_stations,
    fo_land_barrier,
    hotspot_grid,
    snap_tolerance = 650,
    vis_graph = visgraph
  )

  expect_named(out, c("gid", "simulated_count", "modified_count", "dif"))
  expect_s3_class(out, "data.frame")
})

test_that("handles multiple hotspot grids", {
  hotspot_list <- list(
    hotspot_grid,
    create_grid(
      study_area = fo_study_site,
      resolution = 100,
      crs = 3857,
      what = 'polygons'
    )
  )

  out <- compare_tracks(
    tracks,
    fo_stations,
    fo_land_barrier,
    hotspot_list,
    snap_tolerance = 650,
    vis_graph = visgraph
  )

  expect_type(out, "list")
  expect_s3_class(out[[2]], "data.frame")
})


test_that("creates visibility graph if none provided", {
  out <- compare_tracks(
    tracks,
    fo_stations,
    fo_land_barrier,
    hotspot_grid,
    snap_tolerance = 650
  ) |>
    expect_output("Creating visibility graph")
})
