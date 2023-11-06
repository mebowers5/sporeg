#' Calculate mean depth
#' Calculate mean depth per grid cell
#' 
#'
#' @param HSgrid list object of multiple grids
#' @param depth simple feature point object with `"altitude"` attribute data
#'
#' @return A simple feature multipolygon object with mean depth per grid cell
#' @export
#'
#' @examples depth <- readr::read_csv(file = "Depth_Data.csv") %>%
#' sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4269) 
#' depth <- sf::st_filter(depth, site_depth) %>% 
#' mutate(uid = seq_along(geometry)) HS_100km_grid <- depth_data(HS_100km_grid, depth)
depth_data <- function(HSgrid, depth) {
  
  alt <- sf::st_join(HSgrid, depth, join = sf::st_intersects) %>%
    group_by(gid) %>%
    summarise(mean_depth = mean(altitude)*-1)
  
  return(alt)
}
