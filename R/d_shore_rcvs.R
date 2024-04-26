#' Distance to shore and density of receivers
#'
#' This function determines the distance to shore and the density of receivers for each grid cell.
#' @param km grid cell resolution in km. one-sided length of grid cell, assumes desired grid cell is to be squared
#' @param study_site simple feature polygon object that encompasses the entire study site
#' @param land_barrier simple feature (multi)polygon object to route tracks around
#' @param epsg epsg code for desired coordinated system transformation
#' @param sts_pts a simple feature (multi)point object representing receiver locations
#'
#' @return A simple feature multipolygon object with information on distance to shore from grid cell center, receiver presence/absence, counts, and densities
#' @export
#'
d_shore_rcvs <- function(km, study_site, land_barrier, epsg, sts_pts){

  grid_spacing <- km * 1000

  full_grid <- sf::st_make_grid(study_site, square = T, cellsize = c(grid_spacing, grid_spacing)) %>% # Create a grid within the bounding box of the study site
    sf::st_as_sf() %>% # Convert to sf object
    mutate(gid2 = seq_along(x)) # Create ID column for full grid

  grid_centroids <- grid_res(km, study_site, epsg, what = "centers") %>%  # Take the centroids of the grid
    sf::st_as_sf() %>% # Convert to sf object
    mutate(d_shore = as.numeric(sf::st_distance(., land_barrier, by_element = FALSE))) # Find the distance to shore for each grid centroid

  # Correct coastal distances and renumber IDs
  grid_centroids <- grid_centroids %>%
    mutate(d_shore = if_else(sf::st_intersects(., land_barrier, sparse = F), 0, d_shore), # Grid centroids that instersect land have a shore distance of zero
           gid2 = seq_along(x)) %>% # Set an ID column that matches the full grid
    as.data.frame() %>%
    dplyr::select(-x)

  grid <- merge(grid_centroids, full_grid, by = "gid2") %>% # Merge the centroids with the full grid by the shared ID column
    sf::st_as_sf() # Convert to sf object

  grid <- sf::st_intersection(grid, study_site) %>% # Clip the grid to the study site
    mutate(count = lengths(sf::st_intersects(., sts_pts)), # Count the number of receivers in each grid cell
           p_a = as.factor(if_else(count > 0, 1, 0)), # Indicate whether receivers are present or not
           gid = seq_along(gid2), # Re-number grid ID's to equal what the gid's would be if you created the grid inside the study site
           den_rcs = (count/units::set_units(sf::st_area(x), km^2))) # Calculate density of receivers per km^2

  return(grid)
}
