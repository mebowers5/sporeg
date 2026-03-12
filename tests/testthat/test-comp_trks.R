test_that("", {
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
  vis_graph <- pathroutr::prt_visgraph(fo_land_barrier)
  HSgrid <- grid_res(
    100,
    study_site = fo_study_site,
    epsg = 3857,
    what = 'polygons'
  )

  comp_trks(
    tracks,
    fo_stations,
    fo_land_barrier,
    vis_graph,
    HSgrid,
    multi.grid = TRUE,
    snap_tolerance = 650
  )
})
