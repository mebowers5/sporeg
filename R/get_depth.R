#' Calculate mean depth
#'
#' This function allows you to calculate mean depth per grid cell.
#' @param grid list object of multiple grids
#' @param depth simple feature point object with `"altitude"` attribute data
#'
#' @return A simple feature multipolygon object with mean depth per grid cell
#' @export

get_depth <- function(grid, depth, ...) {
  .check_deprecated_dots(dots = list(...))

  alt <- sf::st_join(grid, depth, join = sf::st_intersects) |>
    dplyr::group_by(gid) |>
    dplyr::summarise(mean_depth = mean(altitude) * -1)

  return(alt)
}
