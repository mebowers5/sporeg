#' Re-route tracks around a barrier
#' Re-route demonstrative movement data that has been reconstructed using a movement model around a polygon barrier
#'
#' @param track_data point location data with latitude and longitude information
#' @param CRS epsg code for desired coordinate system transformation
#' @param barrier polygon or multipolygon sf object
#' @param vis_graph vis_graph object created from `pathroutr::prt_vis_graph` function
#' @param buffer desired buffer size for re-routed tracks; generally should relate to range of receivers; distance should use the same units as the final coordinate system
#'
#' @return A simple feature (multi)polygon object 
#'
#' @examples load(file = "subset.Rdata")
#' CRS <- 3857
#' barrier <- sf::st_read("atlcoast.shp") %>%
#' sf::st_transform(., 3857)
#' vis_graph <- pathroutr::prt_visgraph(barrier)
#' buffer <- 650
#' tbuff650 <- sub_rrt(track_data, CRS, barrier, vis_graph, buffer)
#' # Check map
#' mapview::mapView(tbuff650)
#' @export

sub_rrt <- function(track_data, CRS, barrier, vis_graph, buffer) {
  
  # convert the track_data to sf and set the CRS; the bb step is just a way to limit
  # the size of the land polygon and save some computation time when creating vis_graph
  track_path <- track_data %>% sf::st_as_sf(coords = c("mu.x","mu.y"), crs = CRS)
  
  # there are multiple paths identified by ID; we'll group and nest for a proper
  # tidyverse/list-column workflow
  track_path <- track_path %>%
    group_by(ID) %>%
    arrange(ID, time) %>%
    tidyr::nest()
  
  # we need to get our land polygon into a proper format; essentially, we want it to be
  # a series of POLYGONs (and not MULTIPOLYGONs or GEOMETRYCOLLECTION).
  land_barrier <- barrier  %>%
    sf::st_transform(CRS) %>%
    sf::st_collection_extract('POLYGON') %>%
    sf::st_cast('POLYGON')
  
  # prt_visgraph will build our visual graph network from our land barrier object and
  # return a SpatialLinesNetwork / sfNetwork that has no edges that cross land
  # vis_graph <- pathroutr::prt_visgraph(land_barrier)
  
  # the track cannot start or end within the land barrier; prt_trim() trims those out
  track_path <- track_path %>%
    rowwise() %>%
    mutate(trim_data = list(pathroutr::prt_trim(data, land_barrier)))
  
  # here, we create our re-routed points; the return is a two column data frame with the
  # index location in the original point data and the new geometry. The user can handle
  # updating of those original point data or pass the result on to prt_update_points()
  t <- track_path %>% dplyr::rowwise() %>%
    mutate(rrt_pts = list(prt_reroute(trim_data, land_barrier, vis_graph)))
  
  # NOTE: previous versions of prt_update_points() had the argument order reversed from
  # what it now requires. The updated geometry points are passed first (here, `rrt_pts`)
  # and, then, the original data to be updated. This order should allow for easy piping
  # from prt_reroute()
  t <- t %>% dplyr::rowwise() %>%                                                      
    mutate(path_pts = list(prt_update_points(rrt_pts, trim_data)),
           path_lines = list(path_pts %>% summarise(do_union = FALSE) %>% sf::st_cast('LINESTRING')))  # do_union MUST be FALSE!
  
  # we need to rbind all of our lines and points into single objects that can be plotted
  t$geom <- do.call(rbind, t$path_lines)
  t$geom <- sf::st_set_crs(t$geom, CRS)
  
  t <- t %>% dplyr::select(ID, geom) 
  
  #Convert from rowwise_df to sf object
  t <- t %>%
    rowwise() %>%
    mutate(geom = sf::st_geometry(geom)) %>%
    dplyr::select(ID, geom) %>%
    ungroup() %>%
    sf::st_as_sf(., sf_column_name = "geom")
  
  # Buffer re-routed tracks
  tbuff <- t %>%
    sf::st_buffer(., buffer, endCapStyle = "ROUND", joinStyle = "ROUND")
  
  return(tbuff)
}
