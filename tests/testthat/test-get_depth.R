test_that("returns correct types", {
  hotspot_grid <- create_grid(
    study_area = fo_study_site,
    resolution = 50,
    crs = 3857,
    what = 'polygons'
  )

  depth <- sf::st_as_sf(
    depth_data,
    coords = c("longitude", "latitude"),
    crs = 4326
  ) |>
    sf::st_transform(3857)

  get_depth(hotspot_grid, depth) |>
    expect_s3_class(c("sf", "data.frame")) |>
    expect_named(c("gid", "mean_depth", "geometry"))
})
