#' Distance to shore and density of receivers
#'
#' This function determines the distance to shore and the density of receivers for each grid cell.
#' @param sites a simple feature (multi)point object representing receiver locations
#' @param study_area simple feature polygon object that encompasses the entire study site
#' @param resolution grid cell resolution in km. one-sided length of grid cell, assumes desired grid cell is to be squared
#' @param land_barrier simple feature (multi)polygon object to represent the shoreline
#' @param crs specification of desired coordinate reference system transformation
#'
#' @return A simple feature multipolygon object with information on distance to shore from grid cell center, receiver presence/absence, counts, and densities
#' @export
#'
shore_distance <- function(sites, study_area, resolution, land_barrier, crs) {
  grid_spacing <- resolution * 1000

  full_grid <- sf::st_make_grid(
    study_area,
    square = T,
    cellsize = c(grid_spacing, grid_spacing)
  ) |> # Create a grid within the bounding box of the study site
    sf::st_as_sf() |> # Convert to sf object
    dplyr::mutate(gid2 = seq_along(x)) # Create ID column for full grid

  grid_centroids <- create_grid(
    study_area,
    resolution,
    what = "centers",
    crs
  ) |> # Take the centroids of the grid
    sf::st_as_sf() |> # Convert to sf object
    dplyr::mutate(
      d_shore = as.numeric(sf::st_distance(
        geometry,
        land_barrier,
        by_element = FALSE
      ))
    ) # Find the distance to shore for each grid centroid

  # Correct coastal distances and renumber IDs
  grid_centroids <- grid_centroids |>
    dplyr::mutate(
      d_shore = dplyr::if_else(
        as.vector(
          sf::st_intersects(geometry, land_barrier, sparse = F),
        ),
        0,
        d_shore
      ), # Grid centroids that instersect land have a shore distance of zero
      gid2 = seq_along(geometry)
    ) |> # Set an ID column that matches the full grid
    as.data.frame() |>
    dplyr::select(-geometry)

  grid <- merge(grid_centroids, full_grid, by = "gid2") |> # Merge the centroids with the full grid by the shared ID column
    sf::st_as_sf() |> # Convert to sf object
    # set relation attribute to avoid warning: "attribute variables are assumed
    #   to be spatially constant throughout all geometries"
    sf::st_set_agr("constant")

  study_area <- sf::st_set_agr(study_area, "constant")
  sites <- sf::st_set_agr(sites, "constant")

  grid <- sf::st_intersection(grid, study_area) |> # Clip the grid to the study site
    dplyr::mutate(
      count = lengths(sf::st_intersects(x, sites)), # Count the number of receivers in each grid cell
      p_a = as.factor(dplyr::if_else(count > 0, 1, 0)), # Indicate whether receivers are present or not
      gid = seq_along(gid2), # Re-number grid ID's to equal what the gid's would be if you created the grid inside the study site
      den_rcs = (count /
        (units::set_units(
          sf::st_area(x),
          "km^2"
        )) /
        resolution^2)
    ) # Calculate density of receivers per resolution^2

  return(grid)
}
