#' Grid counts
#'
#' The function allows you to calculate counts per grid cell in demonstrative modeled movement data.
#' @param grid spatial grid created from `grid_res` function
#' @param rerouted_tracks sf object of reconstructed, re-routed, optionally buffered tracks created from `sub_rrt` function
#'
#' @return A simple feature object with counts associated with grid cell IDs `"gid"`
#' @export

grid_counts <- function(grid, rerouted_tracks) {
  .check_deprecated_dots(dots = list(...))

  gid <- seq(1:max(grid$gid)) #Create a range of every grid cell
  all <- as.data.frame(gid)

  counts <- sf::st_join(grid, rerouted_tracks, join = sf::st_intersects) |>
    dplyr::distinct(gid, ID, geometry)

  counts <- aggregate(ID ~ gid, data = counts, FUN = length) |>
    dplyr::rename(count = ID)

  counts <- dplyr::left_join(all, counts, by = "gid") |>
    dplyr::mutate_all(~ replace(., is.na(.), 0)) # Replace NA's with zeros so we can check count

  counts <- merge(counts, grid)

  return(counts)
}
