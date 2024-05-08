#' Simulate tracks
#'
#' This function simulates tracks inside a specified polygon.
#' @param anims integer. quantity of desired animals to simulate
#' @param study_site simple feature polygon object that encompasses area where tracks are allowed to be simulated.
#' @param theta argument from `glatos::crw_in_polygon` function
#' @param vmin numeric. minimum velocity from which to sample step length
#' @param vmax numeric. maximum velocity from which to sample step length
#' @param rel_site simple feature polygon object in which simulated animals are "released" or where simulated tracks begin
#' @param crs EPSG code for study_site polygon. Must be projected coordinate system
#' @param n_days integer. number of days that tracks should be simulated
#' @param initHeading argument from `glatos::crw_in_polygon` function
#'
#' @return simple feature (multi)linestring object that represents individual simulated tracks
#' @export
#'
#' @examples
#' library(sporeg)
#' library(sf)
#' library(glatos)
#'
#' load(system.file("extdata", "rel_site.Rda", package = "sporeg"))
#' load(system.file("extdata", "study_site.Rda", package = "sporeg"))
#'
#' anims <- 30
#' yr <- 1
#' theta <- c(0, 1.74)
#' vmin <- 0.98
#' vmax <- 1.58
#' crs <- 3857
#' n_days <- 365*yr
#' initHeading <- 0
#'
#' tracks <- simul_trks(anims, study_site, theta, vmin, vmax, rel_site, crs, n_days, initHeading)

simul_trks <- function(anims, study_site, theta, vmin, vmax, rel_site, crs, n_days, initHeading){

  sim <- base::replicate(n = anims, expr = {

    initPos <- sf::st_sample(rel_site$geometry, size=1, type ="random", exact = TRUE) %>%
      as.data.frame() %>%
      dplyr::mutate(lon = sf::st_coordinates(geometry)[,1],
             lat = sf::st_coordinates(geometry)[,2])

    stepLen <- as.numeric(sample(vmin:vmax, 1)*60*60*24/24) # Sample between minimum and maximum velocity of blacktip sharks to set step length per hour for each individual

    simu <- glatos::crw_in_polygon(study_site,
                                   theta = theta,
                                   stepLen = stepLen,
                                   initPos = c(initPos$lon, initPos$lat),
                                   cartesianCRS = crs,
                                   nsteps = n_days*24, initHeading = initHeading) %>%
      sf::st_as_sf()

    return(simu)
  }
  )

  simu <- sim %>% as.data.frame() %>%
    setNames(gsub("geometry.", perl = TRUE, "", names(.))) %>%
    setNames(gsub("geometry", perl = TRUE, "0", names(.))) %>%
    tidyr::pivot_longer(., cols = tidyr::everything(), names_to = "AnimalID", values_to = "geom") %>%
    dplyr::mutate(ID = as.numeric(AnimalID) + 1) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(uid = seq_along(AnimalID)) #Assign sequential numbers to rows to preserve directionality

  simu <- simu %>%
    dplyr::mutate(POINT_X = sf::st_coordinates(geom)[,1], POINT_Y = sf::st_coordinates(geom)[,2]) %>%  #Obtain x and y coordinates so that you can force northward and southward movement
    dplyr::select(uid,
                  ID,
                  geom, POINT_Y, POINT_X) %>%
    dplyr::group_by(ID) %>%
    dplyr::arrange(uid) %>%
    dplyr::mutate(time = seq.POSIXt(from = as.POSIXct("2000-01-01 01:00:00"), by = "1 hour", along.with = uid),   #Make up dates for the row numbers by Iteration and Animal ID
           start_x = POINT_X, start_y = POINT_Y, end_x = dplyr::lead(POINT_X), end_y = dplyr::lead(POINT_Y)) %>% #Prep data for making lines - ensures proper order
    sf::st_as_sf()

  #Make lines and unite geometries by uid's so that each line segment maintains time information
  simu <- simu %>%
    dplyr::filter(!is.na(end_y)) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data = purrr::map(data,
                        ~ dplyr::mutate(.x,
                                 x = purrr::pmap(.l = list(start_x, start_y, end_x, end_y),
                                                 .f = make_line))))

  return(simu)
}
