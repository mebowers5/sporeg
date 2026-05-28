#' Create grid
#'
#' This function creates a grid inside a specified polygon.
#' @param resolution grid cell resolution in km. one-sided length of grid cell, assumes
#'   desired grid cell is to be squared
#' @param study_site simple feature polygon object in which grid cells should be created
#' @param crs specification for desired coordinate reference system transformation
#' @param type `"polygons"` for grid cell polygons or `"centers"` for center
#'   points of grid cells
#'
#' @return grid comprised of polygons or points depending on `type` parameter
#' @export
#'
#' @examples
#' # NOTE: The study site must be in a projected coordinate system (e.g. EPSG: 3857)
#' # when it is initially fed into the function create_grid
#'
#' site_depth_3857 <- site_depth |> sf::st_transform(3857)
#' HS_100km_grid <- create_grid(100, site_depth_3857, 4269, "polygons")

create_grid <- function(resolution, study_site, crs, type) {
  grid_spacing <- resolution * 1000 # Desired kilometers times 1000 m per 1 km

  if (sf::st_is_longlat(study_site)) {
    stop("Study site is not in a projected coordinate system.")
  }

  if (type == "polygons") {
    grid <- sf::st_make_grid(
      study_site,
      square = T,
      what = type,
      cellsize = c(grid_spacing, grid_spacing)
    ) |> # Create a grid inside the coastal 500 m isobath polygon
      sf::st_as_sf() |>
      dplyr::mutate(gid = seq_along(x)) |>
      sf::st_make_valid() |>
      sf::st_transform(crs) |>
      sf::st_cast('MULTIPOLYGON')

    grid <- sf::st_intersection(
      study_site |> sf::st_transform(crs),
      grid
    ) |>
      sf::st_as_sf() |>
      dplyr::mutate(gid = seq_along(geometry)) |>
      sf::st_make_valid() |>
      sf::st_transform(crs) |>
      sf::st_cast('MULTIPOLYGON')

    return(grid)
  } else if (type == "centers") {
    grid <- sf::st_make_grid(
      study_site,
      square = T,
      what = type,
      cellsize = c(grid_spacing, grid_spacing)
    ) |> # Create a grid inside the coastal 500 m isobath polygon
      sf::st_as_sf() |>
      dplyr::mutate(gid = seq_along(x)) |>
      sf::st_make_valid() |>
      sf::st_transform(crs) |>
      sf::st_cast('POINT')

    return(grid)
  } else {
    stop("Error: Not a valid argument for parameter 'type'")
  }
}
