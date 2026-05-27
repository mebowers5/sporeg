#' Create grid
#'
#' This function creates a grid inside a specified polygon.
#' @param km grid cell resolution in km. one-sided length of grid cell, assumes
#'   desired grid cell is to be squared
#' @param study_site simple feature polygon object in which grid cells should be created
#' @param epsg epsg numeric code for desired coordinate system transformation
#' @param what `"polygons"` for grid cell polygons or `"centers"` for center
#'   points of grid cells
#' @param simplify numeric. By what proportion of `km` do you wish to simplify
#'   the `study_site` polygon? Higher numbers give faster run times at the expense
#'   of resolution. Defaults to 0.2.
#'
#' @return grid comprised of polygons or points depending on `what` parameter
#' @export
#'
#' @examples
#' # NOTE: The study site must be in a projected coordinate system (e.g. EPSG: 3857)
#' # when it is initially fed into the function grid_res
#'
#' site_depth_3857 <- site_depth |> sf::st_transform(3857)
#' HS_100km_grid <- grid_res(100, site_depth_3857, 4269, "polygons")

grid_res <- function(
  km,
  study_site,
  epsg,
  what = c("polygons", "centers"),
  simplify = 0.2
) {
  what <- rlang::arg_match0(what, c("polygons", "centers"))

  grid_spacing <- km * 1000 # Desired kilometers times 1000 m per 1 km

  if (sf::st_is_longlat(study_site)) {
    stop("Study site is not in a projected coordinate system.")
  }

  grid <- study_site |>
    # Create a grid inside the study site polygon
    sf::st_make_grid(
      square = T,
      what = what,
      cellsize = c(grid_spacing, grid_spacing)
    ) |>
    sf::st_as_sf() |>
    dplyr::mutate(gid = seq_along(x)) |>
    sf::st_make_valid() |>
    sf::st_transform(epsg) |>
    dplyr::rename(geometry = "x")

  if (what == "polygons") {
    grid <- grid |>
      sf::st_cast('MULTIPOLYGON')

    # Simplify polygon by 20% of the grid spacing to speed up st_intersection
    study_site_proj <- study_site |>
      sf::st_transform(epsg) |>
      st_simplify(preserveTopology = TRUE, dTolerance = grid_spacing * 0.2)

    grid <- sf::st_intersection(
      study_site_proj,
      grid
    ) |>
      sf::st_as_sf() |>
      dplyr::mutate(gid = seq_along(geometry)) |>
      sf::st_make_valid() |>
      sf::st_transform(epsg) |>
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
