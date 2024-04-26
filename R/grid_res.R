#' Create grid
#'
#' This function creates a grid inside a specified polygon.
#' @param km grid cell resolution in km. one-sided length of grid cell, assumes desired grid cell is to be squared
#' @param study_site simple feature polygon object in which grid cells should be created
#' @param epsg epsg numeric code for desired coordinate system transformation
#' @param what `"polygons"` for grid cell polygons or `"centers"` for center points of grid cells
#'
#' @return grid comprised of polygons or points depending on `what` parameter
#' @export
#'
#' @examples
#' site_depth <- sf::st_read("Bathy500m.shp") %>% sf::st_transform(4269)
#' # NOTE: The study site must be in a projected coordinate system (e.g., WGS 84; EPSG: 3857)
#' # when it is initially fed into the function grid_res
#' site_depth <- site_depth %>% sf::st_transform(., 3857)
#' HS_100km_grid <- grid_res(100, site_depth, 4269, "polygons")

grid_res <- function(km, study_site, epsg, what) {

  grid_spacing <- km * 1000 # Desired kilometers times 1000 m per 1 km

  ifelse(what == "polygons", {

    grid <- sf::st_make_grid(study_site, square = T, what = what, cellsize = c(grid_spacing, grid_spacing)) %>% # Create a grid inside the coastal 500 m isobath polygon
      sf::st_as_sf() %>%
      dplyr::mutate(gid = seq_along(x)) %>%
      sf::st_make_valid() %>%
      sf::st_transform(epsg) %>%
      sf::st_cast('MULTIPOLYGON')

    grid <- sf::st_intersection(study_site %>% sf::st_transform(epsg), grid) %>%
      sf::st_as_sf() %>%
      dplyr::mutate(gid = seq_along(geometry)) %>%
      sf::st_make_valid() %>%
      sf::st_transform(epsg) %>%
      sf::st_cast('MULTIPOLYGON')

    return(grid)
  }, ifelse(what == "centers",
            {
              grid <- sf::st_make_grid(study_site, square = T, what = what, cellsize = c(grid_spacing, grid_spacing)) %>% # Create a grid inside the coastal 500 m isobath polygon
                sf::st_as_sf() %>%
                dplyr::mutate(gid = seq_along(x)) %>%
                sf::st_make_valid() %>%
                sf::st_transform(epsg) %>%
                sf::st_cast('POINT')

              return(grid)
            },
            print("Error: Not a valid argument for parameter 'what'")
  ))
}
