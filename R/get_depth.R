#' Calculate mean depth
#'
#' This function allows you to calculate mean depth per grid cell.
#' @param hotspot_grid list object of multiple grids
#' @param depth simple feature point object with `"altitude"` attribute data
#'
#' @return A simple feature multipolygon object with mean depth per grid cell
#' @export

get_depth <- function(hotspot_grid, depth) {
  row.names(hotspot_grid) <- NULL

  hotspot_grid$mean_depth <- sf::st_intersects(hotspot_grid, depth) |>
    sapply(
      function(grid_cell) {
        if (length(grid_cell) == 0) {
          NA
        } else {
          mean(depth[grid_cell, ]$altitude, na.rm = T) * -1
        }
      }
    )

  hotspot_grid[, c("gid", "mean_depth")]
}
