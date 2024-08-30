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
#' library(dplyr)
#' library(sf)
#' library(tidyr)
#'
#' load(system.file("extdata", "at_dly_locs.Rda", package = "sporeg"))
#'
#' at_lines <- at_dly_locs %>%
#' group_by(ID, time) %>%
#' sf::st_transform(3857) %>%
#' summarise(pt = sf::st_combine(geometry)) %>%
#' sf::st_centroid() %>%
#' mutate(lat = sf::st_coordinates(pt)[,2],
#' lon = sf::st_coordinates(pt)[,1]) %>%
#' arrange(ID, time) %>%
#' mutate(start_x = lon, start_y = lat, end_x = lead(lon), end_y = lead(lat)) %>% #Prep data for making lines - ensures proper order
#' sf::st_as_sf(coords = c("lon", "lat"), crs = 3857) %>%
#' filter(!is.na(end_y)) %>%
#' tidyr::nest() %>%
#' mutate(
#' data = purrr::map(data,
#' ~ mutate(.x,
#' x = purrr::pmap(.l = list(start_x, start_y, end_x, end_y),
#' .f = make_line)))) %>%
#' mutate(
#' data = purrr::map(data,
#' ~ mutate(.x, x = sf::st_sfc(x))),
#' x = purrr::map(data, ~ sf::st_union(sf::st_set_geometry(.x, 'x'))), ##Preserves order
#' x = purrr::map(x, ~ sf::st_cast(.x, 'MULTILINESTRING'))
#' ) %>%
#' dplyr::select(-data) %>%
#' tidyr::unnest(x) %>%
#' sf::st_as_sf(sf_column_name = 'x', crs = 3857)

make_line <- function(start_x, start_y, end_x, end_y) {
  sf::st_linestring(matrix(c(start_x, end_x, start_y, end_y), 2, 2))
}
