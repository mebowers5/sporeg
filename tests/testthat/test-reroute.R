df <- demo_dets |>
  dplyr::filter(ID %in% 1:2) |>
  sf::st_transform(3857)
df_small <- df |>
  dplyr::filter(ID == "1", study_yr == "year_5")

vis_graph <- pathroutr::prt_visgraph(atlcoast)


test_that("works with no grouping", {
  out <- reroute(df = df_small, barrier = atlcoast, vis_graph = vis_graph)

  expect_s3_class(out, c("sf", "data.frame"))
  expect_true(sf::st_geometry_type(out) == "LINESTRING")
})

test_that("works with one grouping level", {
  out <- df |>
    dplyr::filter(study_yr == "year_5") |>
    dplyr::group_by(ID) |>
    reroute(barrier = atlcoast, vis_graph = vis_graph)

  expect_s3_class(out, c("sf", "data.frame"))
  expect_true(all(sf::st_geometry_type(out) == "LINESTRING"))
  expect_shape(out, dim = c(2, 2))
  expect_equal(out$ID, c(1, 2))
})

test_that("works with more than one grouping level", {
  out <- df |>
    dplyr::group_by(ID, study_yr) |>
    reroute(barrier = atlcoast, vis_graph = vis_graph)

  expect_s3_class(out, c("sf", "data.frame"))
  expect_true(all(sf::st_geometry_type(out) == "LINESTRING"))
  expect_shape(out, dim = c(2, 2))
  expect_equal(out$ID, c(1, 2))
})

test_that("creates visibility graph if none provided", {
  out <- reroute(df = df_small, barrier = atlcoast) |>
    expect_output("Creating visibility graph")
})
