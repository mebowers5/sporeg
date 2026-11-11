#' Create grid
#'
#' This function creates a grid inside a specified polygon.
#' @param study_area simple feature polygon object in which grid cells should be created
#' @param resolution grid cell resolution in km. one-sided length of grid cell, assumes
#'   desired grid cell is to be squared
#' @param what `"polygons"` for grid cell polygons or `"centers"` for center
#'   points of grid cells
#' @param crs specification of desired coordinate reference system transformation
#'
#' @return grid comprised of polygons or points depending on `what` parameter
#' @export
#'
#' @examples
#' # NOTE: The study area must be in a projected coordinate system (e.g., EPSG: 3857)
#' # when it is initially fed into the function create_grid
#'
#' HS_100km_grid <- create_grid(study_area, 100, "centers", 4269)

create_grid <- function(study_area, resolution, what, crs) {
  if (sf::st_is_longlat(study_area)) {
    stop("Study area is not in a projected coordinate system.")
  }

  grid_spacing <- resolution * 1000 # Desired kilometers times 1000 m per 1 km

  grid <- sf::st_make_grid(
    study_area,
    square = T,
    what = what,
    cellsize = c(grid_spacing, grid_spacing)
  ) |> # Create a grid inside the coastal 500 m isobath polygon
    sf::st_as_sf() |>
    dplyr::mutate(gid = seq_along(x)) |>
    dplyr::rename(geometry = x) |>
    sf::st_make_valid() |>
    sf::st_transform(crs)

  if (what == "polygons") {
    grid <- grid |>
      sf::st_cast('MULTIPOLYGON')

    grid <- study_area |>
      sf::st_transform(crs) |>
      sf::st_intersection(grid) |>
      sf::st_as_sf() |>
      dplyr::mutate(gid = seq_along(geometry)) |>
      sf::st_make_valid() |>
      sf::st_cast('MULTIPOLYGON')

    return(grid)
  } else if (what == "centers") {
    grid <- grid |>
      sf::st_cast('POINT')

    return(grid)
  } else {
    stop("Error: Not a valid argument for parameter 'what'")
  }
}
