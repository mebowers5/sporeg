#' Calculate mean depth
#'
#' This function allows you to calculate mean depth per grid cell.
#' @param HSgrid list object of multiple grids
#' @param depth simple feature point object with `"altitude"` attribute data
#'
#' @return A simple feature multipolygon object with mean depth per grid cell
#' @export

depth_data <- function(HSgrid, depth) {

  alt <- sf::st_join(HSgrid, depth, join = sf::st_intersects) %>%
    dplyr::group_by(gid) %>%
    dplyr::summarise(mean_depth = mean(altitude)*-1)

  return(alt)
}
