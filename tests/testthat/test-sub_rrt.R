test_that("returns correct types", {
  vis_graph <- pathroutr::prt_visgraph(atlcoast)
  track_data <- sporeg::subset |>
    dplyr::filter(ID %in% c("A69-9001-23338", "A69-9001-25494"))

  res <- sub_rrt(track_data, 3857, atlcoast, vis_graph, 650)

  expect_s3_class(res, c("sf", "data.frame"))
  expect_s3_class(res$geom, c("sfc_POLYGON", "sfc"))
})
