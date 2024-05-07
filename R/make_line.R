#' Make a line
#'
#' This functions creates a line by connecting start and end points.
#' @param start_x longitudinal sf coordinate object from the desired start point
#' @param start_y latitudinal sf coordinate object from the desired start point
#' @param end_x longitudinal sf coordinate object from the desired end point
#' @param end_y latitudinal sf coordinate object from the desired end point
#'
#' @return A simple feature geometry object or simple feature linestring object
#' @keywords internal
#' @noRd
#' @export


make_line <- function(start_x, start_y, end_x, end_y) {
  sf::st_linestring(matrix(c(start_x, end_x, start_y, end_y), 2, 2))
}
