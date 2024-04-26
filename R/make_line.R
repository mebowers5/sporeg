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
#' make_line(Longitude, Latitude, lead(Longitude), lead(Latitude))
#' make_line(sf::st_coordinates(point_geom)[,1], sf::st_coordinates(point_geom)[,2],
#' lead(sf::st_coordinates(point_geom)[,1]), lead(sf::st_coordinates(point_geom)[,2]))
#' @export
make_line <- function(start_x, start_y, end_x, end_y) {
  sf::st_linestring(matrix(c(start_x, end_x, start_y, end_y), 2, 2))
}
