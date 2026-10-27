test_that("", {
  tracks <- simul_trks(
    anims = 2,
    study_site = fo_study_site,
    theta = c(0, 1.74),
    vmin = 0.98,
    vmax = 1.58,
    rel_site = fo_rel_site,
    epsg = 3857,
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

  sim_trks <- tracks
  stations <- fo_stations
  land_barrier <- fo_land_barrier
  cores <- 2
  snap_tolerance <- 650

  mm <- mod_trks[1, ]
  trace(
    "loadNamespace",
    tracer = quote(if (package == "sf") browser()),
    print = FALSE
  )
  pathroutr::prt_reroute(trkpts = mm$trim_data[[1]], land_barrier, vis_graph)

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
