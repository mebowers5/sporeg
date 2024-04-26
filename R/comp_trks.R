# Compare simulated and modeled tracks
#' Compare grid cell counts
#'
#' @param sim_trks Tracks to which modeled tracks should be compared
#' @param stations A sf object comprised of receiver station locations with a buffer around them that represents the range of the receiver (polygons)
#' @param land_barrier A sf polygon of a barrier object around which tracks should be re-routed
#' @param vis_graph A visibility graph created from the barrier object @seealso [pathroutr::prt_visgraph]
#' @param multi.grid boolean
#' @param HSgrid When multi.grid = FALSE, a sf polygon grid; When multi.grid = TRUE, a list of sf polygon grids
#' @param snap_tolerance The tolerance (in meters) at which an intersection between a station and a track should snap to the station centroid. It is recommended that the snap_tolerance be equal to the station buffer size.
#'
#' @return A data frame with counts and differences by grid cell ID ("gid")
#' @export
comp_trks <- function(sim_trks, stations, land_barrier, vis_graph, multi.grid, HSgrid, snap_tolerance) {

  if(multi.grid == FALSE) {

  # Maintain detailed time info in lines to derive acoustic telemetry detection data later
  mod_trks <- sim_trks %>%
    tidyr::unnest(data) %>%
    dplyr::mutate(x = sf::st_sfc(x)) %>%
    sf::st_as_sf(sf_column_name = 'x', crs = 3857)

  #Unite geometries by AnimalID's so that you have complete tracks/lines summarized by ID for the grid cell count later
  sim_trks <- sim_trks %>%
    dplyr::mutate(
      data = purrr::map(data,
                        ~ dplyr::mutate(.x, x = sf::st_sfc(x))),
      x = purrr::map(data, ~ sf::st_union(sf::st_set_geometry(.x, 'x'))), ##Preserves order
      x = purrr::map(x, ~ sf::st_cast(.x, 'MULTILINESTRING'))
    ) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(x) %>%
    sf::st_as_sf(sf_column_name = 'x', crs = 3857)

  ### Find intersections of tracks and receiver stations
  # These intersections represent data that a researcher might receive from an animal tagged with an acoustic transmitter that traveled along the track that we created

  # Derive acoustic detection data through the intersection of lines and receiver ranges
  # Cannot just take the intersection because the lines self intersect
  # Take the symmetrical difference between the lines and self intersection to get the true derived detection data
  mod_trks <- sf::st_intersection(mod_trks, stations$geometry) %>%  # x provides intersection geometry - linestrings that cross station polygons
    sf::st_centroid() # Take the centroid of the linestrings

  mod_trks <- sf::st_snap(mod_trks, sf::st_centroid(stations$geometry), tolerance = snap_tolerance) # Snap the linestring centroids to the station point geometry - geometry 'x' is altered

  ### Run "derived acoustic telemetry data through continuous time correlated random walk model
  #Load data----

  #In real data, problem caused by the zero movement between locations at the same acoustic receiver, close together in time - solved by averaging daily locations
  #Average daily locations
  mod_trks <- mod_trks %>%
    dplyr::arrange(ID, time) %>%
    dplyr::group_by(ID, time) %>%
    dplyr::summarize(mean_pos = sf::st_combine(x)) %>%
    sf::st_centroid() %>%
    dplyr::mutate(time = as.POSIXct(time),
           x = sf::st_coordinates(mean_pos)[,1],
           y = sf::st_coordinates(mean_pos)[,2],
           locType = "o") %>% # Obtain x and y coordinates so that you can merge with predicted locations later
    sf::st_drop_geometry() %>%
    as.data.frame

  #Impute missing locations with continuous time movement model
  mod_trks <- momentuHMM::crawlWrap(obsData = mod_trks, timeStep = "15 min",
                                    fixPar = c(NA, NA),
                                    theta = c(8,0),
                                    attempts = 100, retrySD = 5, retryFits = 5,
                                    ncores = 2, retryParallel = TRUE)

  #Continue with movement model data products
  #Get the data.frame of predicted locations
  mod_trks <- data.frame(mod_trks$crwPredict)

  #Turn crwPredict product into data frame
  mod_trks <- mod_trks %>%
    dplyr::select(ID, locType, time, mu.x, mu.y, speed) %>%
    dplyr::mutate(locType = ifelse(is.na(locType), "p", locType)) %>%  #Specify predicted locs
    as.data.frame

  # Read in the track data; here, I'm filtering to just include the predicted locations
  # and making sure the time is a proper POSIX. Neither of those two steps are
  # necessary
  mod_trks <- mod_trks %>%
    #dplyr::filter(locType == "p") %>%
    dplyr::select(ID, time, mu.x, mu.y) %>%
    dplyr::mutate(time = lubridate::ymd_hms(format(as.POSIXct(time, tz = "UTC"), "%Y-%m-%d %H:%M:%S", ID = as.factor(ID))))

  # convert the track_data to sf and set the CRS; the bb step is just a way to limit
  # the size of the land polygon and save some computation time when creating vis_graph
  mod_trks <- mod_trks %>% sf::st_as_sf(coords = c("mu.x","mu.y"), crs = 3857)
  #bb <- sf::st_as_sfc(sf::st_bbox(track_path))

  # there are multiple paths identified by ID; we'll group and nest for a proper
  # tidyverse/list-column workflow
  mod_trks <- mod_trks %>%
    dplyr::group_by(ID) %>%
    tidyr::nest()

  # the track cannot start or end within the land barrier; prt_trim() trims those out
  mod_trks <- mod_trks %>%
    dplyr::rowwise() %>%
    dplyr::mutate(trim_data = list(pathroutr::prt_trim(data, land_barrier)))

  # here, we create our re-routed points; the return is a two column data frame with the
  # index location in the original point data and the new geometry. The user can handle
  # updating of those original point data or pass the result on to prt_update_points()
  mod_trks <- mod_trks %>% dplyr::rowwise() %>%
    dplyr::mutate(rrt_pts = list(pathroutr::prt_reroute(trim_data, land_barrier, vis_graph)))

  # Clear cache
  gc()

  # NOTE: previous versions of prt_update_points() had the argument order reversed from
  # what it now requires. The updated geometry points are passed first (here, `rrt_pts`)
  # and, then, the original data to be updated. This order should allow for easy piping
  # from prt_reroute()
  mod_trks <- mod_trks %>% dplyr::rowwise() %>%
    dplyr::mutate(path_pts = list(pathroutr::prt_update_points(rrt_pts, trim_data)),
           path_lines = list(path_pts %>% dplyr::summarise(do_union = FALSE) %>% sf::st_cast('LINESTRING')))  # do_union MUST be FALSE!

  # Clear cache
  gc()

  # we need to rbind all of our lines and points into single objects that can be plotted
  mod_trks$geom <- do.call(rbind, mod_trks$path_lines)
  mod_trks$geom <- sf::st_set_crs(mod_trks$geom, 3857)

  mod_trks <- mod_trks %>% dplyr::select(ID, geom)

  ### Create grid that maintains row for each grid cell
  #Convert from rowwise_df to sf object
  mod_trks <- mod_trks %>%
    dplyr::rowwise() %>%
    dplyr::mutate(geom = sf::st_geometry(geom)) %>%
    dplyr::select(ID, geom) %>%
    dplyr::ungroup() %>%
    sf::st_as_sf(., sf_column_name = "geom")

  ### Count distinct AnimalIDs in each grid cell
  # Count IDs per grid cell for derived data
  mod_count <- sf::st_join(HSgrid, mod_trks, join = sf::st_intersects) %>%
    dplyr::distinct(gid, ID, geometry)

  mod_count <- aggregate(ID ~ gid, data = mod_count, FUN = length) %>%
    dplyr::rename(mod_count = ID)

  # Count IDs per grid cell for complete data
  sim_count <- sf::st_join(HSgrid, sim_trks, join =sf::st_intersects)%>%
    dplyr::distinct(gid, ID, geometry)

  sim_count <- aggregate(ID ~ gid, data = sim_count, FUN = length) %>%
    dplyr::rename(sim_count = ID)

  #Make a new table with all gid's to create a basis by which tables should be merged
  gid <- seq(1:max(HSgrid$gid))  #Create a range of every grid cell
  all <- as.data.frame(gid)  #Create every combination of iteration and gid

  #Now that we have every combination of gid and Iteration in one variable, merge one of the count files to it - doesn't really matter which on but we'll use the complete data one here
  # Note: Is the above comment wrong or is the code wrong? I think the comment is wrong - leftover from when iterations were assigned instead of looped.
  tc <- dplyr::left_join(all, sim_count, by = "gid") %>%
    dplyr::mutate_all(~replace(., is.na(.), 0))  # Replace NA's with zeros so we can check count

  #Join derived count to data frame
  tc <- dplyr::left_join(tc, mod_count, by = "gid") %>%
    dplyr::mutate_all(~replace(., is.na(.), 0))  # Replace NA's with zeros so we can check count

  #Calculate differences in each grid cell by iteration and gid
  tc <- tc %>%
    dplyr::select(gid, sim_count, mod_count) %>%
    dplyr::mutate(sim_count = as.double(sim_count),
           mod_count = as.double(mod_count),
           dif = sim_count - mod_count) %>%
    dplyr::arrange(gid) %>%
    dplyr::select(gid, sim_count, mod_count, dif) %>%
    as.data.frame

  return(tc)
  }else
  {
  # Maintain detailed time info in lines to derive acoustic telemetry detection data later
  mod_trks <- sim_trks %>%
    tidyr::unnest(data) %>%
    dplyr::mutate(x = sf::st_sfc(x)) %>%
    sf::st_as_sf(sf_column_name = 'x', crs = 3857)

  #Unite geometries by AnimalID's so that you have complete tracks/lines summarized by ID for the grid cell count later
  sim_trks <- sim_trks %>%
    dplyr::mutate(
      data = purrr::map(data,
                        ~ dplyr::mutate(.x, x = sf::st_sfc(x))),
      x = purrr::map(data, ~ sf::st_union(sf::st_set_geometry(.x, 'x'))), ##Preserves order
      x = purrr::map(x, ~ sf::st_cast(.x, 'MULTILINESTRING'))
    ) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(x) %>%
    sf::st_as_sf(sf_column_name = 'x', crs = 3857)

  ### Find intersections of tracks and receiver stations
  # These intersections represent data that a researcher might receive from an animal tagged with an acoustic transmitter that traveled along the track that we created

  # Derive acoustic detection data through the intersection of lines and receiver ranges
  # Cannot just take the intersection because the lines self intersect
  # Take the symmetrical difference between the lines and self intersection to get the true derived detection data
  mod_trks <- sf::st_intersection(mod_trks, stations$geometry) %>%  # x provides intersection geometry - linestrings that cross station polygons
    sf::st_centroid() # Take the centroid of the linestrings

  mod_trks <- sf::st_snap(mod_trks, sf::st_centroid(stations$geometry), tolerance = snap_tolerance) # Snap the linestring centroids to the station point geometry - geometry 'x' is altered

  ### Producing duplicate locations here... not the problem but need to rectify later

  ### Run "derived acoustic telemetry data through continuous time correlated random walk model
  #Load data----

  #In real data, problem caused by the zero movement between locations at the same acoustic receiver, close together in time - solved by averaging daily locations
  #Average daily locations
  mod_trks <- mod_trks %>%
    dplyr::arrange(ID, time) %>%
    dplyr::group_by(ID, time) %>%
    dplyr::summarize(mean_pos = sf::st_combine(x)) %>%
    sf::st_centroid() %>%
    dplyr::mutate(time = as.POSIXct(time),
           x = sf::st_coordinates(mean_pos)[,1],
           y = sf::st_coordinates(mean_pos)[,2],
           locType = "o") %>% # Obtain x and y coordinates so that you can merge with predicted locations later
    sf::st_drop_geometry() %>%
    as.data.frame

  #Impute missing locations with continuous time movement model
  mod_trks <- momentuHMM::crawlWrap(obsData = mod_trks, timeStep = "15 min",
                                      fixPar = c(NA, NA),
                                      theta = c(8,0),
                                   attempts = 100, retrySD = 5, retryFits = 5,
                                   ncores = 2, retryParallel = TRUE)

  #Continue with movement model data products
  #Get the data.frame of predicted locations
  mod_trks <- data.frame(mod_trks$crwPredict)

  #Turn crwPredict product into data frame
  mod_trks <- mod_trks %>%
    dplyr::select(ID, locType, time, mu.x, mu.y, speed) %>%
    dplyr::mutate(locType = ifelse(is.na(locType), "p", locType)) %>%  #Specify predicted locs
    as.data.frame

  # Read in the track data; here, I'm filtering to just include the predicted locations
  # and making sure the time is a proper POSIX. Neither of those two steps are
  # necessary
  mod_trks <- mod_trks %>%
    #dplyr::filter(locType == "p") %>%
    dplyr::select(ID, time, mu.x, mu.y) %>%
    dplyr::mutate(time = lubridate::ymd_hms(format(as.POSIXct(time, tz = "UTC"), "%Y-%m-%d %H:%M:%S", ID = as.factor(ID))))

  # convert the track_data to sf and set the CRS; the bb step is just a way to limit
  # the size of the land polygon and save some computation time when creating vis_graph
  mod_trks <- mod_trks %>% sf::st_as_sf(coords = c("mu.x","mu.y"), crs = 3857)
  #bb <- sf::st_as_sfc(sf::st_bbox(track_path))

  # there are multiple paths identified by ID; we'll group and nest for a proper
  # tidyverse/list-column workflow
  mod_trks <- mod_trks %>%
    dplyr::group_by(ID) %>%
    tidyr::nest()

  # the track cannot start or end within the land barrier; prt_trim() trims those out
  mod_trks <- mod_trks %>%
    dplyr::rowwise() %>%
    dplyr::mutate(trim_data = list(pathroutr::prt_trim(data, land_barrier)))

  # here, we create our re-routed points; the return is a two column data frame with the
  # index location in the original point data and the new geometry. The user can handle
  # updating of those original point data or pass the result on to prt_update_points()
  mod_trks <- mod_trks %>% dplyr::rowwise() %>%
    dplyr::mutate(rrt_pts = list(pathroutr::prt_reroute(trim_data, land_barrier, vis_graph)))

  # NOTE: previous versions of prt_update_points() had the argument order reversed from
  # what it now requires. The updated geometry points are passed first (here, `rrt_pts`)
  # and, then, the original data to be updated. This order should allow for easy piping
  # from prt_reroute()
  mod_trks <- mod_trks %>% dplyr::rowwise() %>%
    dplyr::mutate(path_pts = list(pathroutr::prt_update_points(rrt_pts, trim_data)),
           path_lines = list(path_pts %>% dplyr::summarise(do_union = FALSE) %>% sf::st_cast('LINESTRING')))  # do_union MUST be FALSE!

  # we need to rbind all of our lines and points into single objects that can be plotted
  mod_trks$geom <- do.call(rbind, mod_trks$path_lines)
  mod_trks$geom <- sf::st_set_crs(mod_trks$geom, 3857)

  mod_trks <- mod_trks %>% dplyr::select(ID, geom)

  ### Create grid that maintains row for each grid cell
  #Convert from rowwise_df to sf object
  mod_trks <- mod_trks %>%
    dplyr::rowwise() %>%
    dplyr::mutate(geom = sf::st_geometry(geom)) %>%
    dplyr::select(ID, geom) %>%
    dplyr::ungroup() %>%
    sf::st_as_sf(., sf_column_name = "geom")


  ### Need to create workflow to handle multiple grid cell resolutions
  # Apply function over list of grid cell HSgrid's
  results <- lapply(HSgrid, FUN = function (HSgrid) {


  ### Count distinct AnimalIDs in each grid cell
  # Count IDs per grid cell for derived data
  mod_count <- sf::st_join(HSgrid, mod_trks, join = sf::st_intersects) %>%
    dplyr::distinct(gid, ID, geometry)

  mod_count <- aggregate(ID ~ gid, data = mod_count, FUN = length) %>%
    dplyr::rename(mod_count = ID)

  # Count IDs per grid cell for complete data
  sim_count <- sf::st_join(HSgrid, sim_trks, join =sf::st_intersects)%>%
    dplyr::distinct(gid, ID, geometry)

  sim_count <- aggregate(ID ~ gid, data = sim_count, FUN = length) %>%
    dplyr::rename(sim_count = ID)

  #Make a new table with all gid's to create a basis by which tables should be merged
  gid <- seq(1:max(HSgrid$gid))  #Create a range of every grid cell
  all <- as.data.frame(gid)  #Create every combination of iteration and gid

  #Now that we have every combination of gid and Iteration in one variable, merge one of the count files to it - doesn't really matter which on but we'll use the complete data one here
  # Note: Is the above comment wrong or is the code wrong? I think the comment is wrong - leftover from when iterations were assigned instead of looped.
  tc <- dplyr::left_join(all, sim_count, by = "gid") %>%
    dplyr::mutate_all(~replace(., is.na(.), 0))  # Replace NA's with zeros so we can check count

  #Join derived count to data frame
  tc <- dplyr::left_join(tc, mod_count, by = "gid") %>%
    dplyr::mutate_all(~replace(., is.na(.), 0))  # Replace NA's with zeros so we can check count

  #Calculate differences in each grid cell by iteration and gid
  tc <- tc %>%
    dplyr::select(gid, sim_count, mod_count) %>%
    dplyr::mutate(sim_count = as.double(sim_count),
           mod_count = as.double(mod_count),
           dif = sim_count - mod_count) %>%
    dplyr::arrange(gid) %>%
    dplyr::select(gid, sim_count, mod_count, dif) %>%
    as.data.frame

  }
  )
  return(results)

}

}
