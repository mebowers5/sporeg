# test_that("returns correct types", {

#   fo_sts_pts <- fo_stations |>
#     sf::st_centroid()

#   fo_land_barrier <- fo_land_barrier |>
#     sf::st_transform(3857) |>
#     sf::st_union() |>
#     sf::st_as_sf()

#   result <- d_shore_rcvs(100, fo_study_site, fo_land_barrier, 3857, fo_sts_pts)

#   result |>
#     expect_s3_class(c("sf", "data.frame")) |>
#     expect_named(
#       c("gid2", "gid", "d_shore", "poly_uid", "x", "count", "p_a", "den_rcs")
#     )
#   expect_s3_class(result$x, c("sfc_GEOMETRY", "sfc"), exact = TRUE)
#   expect_s3_class(result$den_rcs, "units")
# })
