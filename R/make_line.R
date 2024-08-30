#' Make a line
#'
#' This functions creates a line by connecting start and end points.
#' @param start_x longitudinal sf coordinate object from the desired start point
#' @param start_y latitudinal sf coordinate object from the desired start point
#' @param end_x longitudinal sf coordinate object from the desired end point
#' @param end_y latitudinal sf coordinate object from the desired end point
#'
#' @return A simple feature geometry object or simple feature linestring object
#'
#' @examples
#' library(sporeg)
#' library(dplyr)
#' library(sf)
#' library(purrr)
#' library(tidyr)
#'
#' load(system.file("extdata", "at_dly_locs.Rda", package = "sporeg"))
#'
#'at_lines <- at_dly_locs %>%
#'  dplyr::group_by(ID, time) %>%
#'  sf::st_transform(3857) %>%
#'  dplyr::summarise(pt = sf::st_combine(geometry)) %>%
#'  sf::st_centroid() %>%
#'  dplyr::mutate(lat = sf::st_coordinates(pt)[,2],
#'         lon = sf::st_coordinates(pt)[,1]) %>%
#'  dplyr::arrange(ID, time) %>% #Order data for making lines
#'  dplyr::mutate(start_x = lon, start_y = lat,
#'  end_x = dplyr::lead(lon), end_y = dplyr::lead(lat)) %>%
#'  sf::st_as_sf(coords = c("lon", "lat"), crs = 3857) %>%
#'  dplyr::filter(!is.na(end_y)) %>%
#'  tidyr::nest() %>%
#'  dplyr::mutate(
#'    data = purrr::map(data,
#'                      ~ dplyr::mutate(.x,
#'                               x = purrr::pmap(.l = list(start_x, start_y, end_x, end_y),
#'                                               .f = make_line))))

make_line <- function(start_x, start_y, end_x, end_y) {
  sf::st_linestring(matrix(c(start_x, end_x, start_y, end_y), 2, 2))
}
