#' Generate random receivers within a polygon
#'
#' This functions creates a line by connecting start and end points.
#' @param study_site simple feature polygon object that encompasses the entire study site; must be in projected coordinate system for receiver range to be in meters
#' @param quantity desired number of receivers to generate
#' @param receiver_range radius of receiver range in number of meters
#' @param view see a map of receivers after they are generated
#'
#' @return A simple feature geometry object
#' @export
#'
#' @examples
#'
#' study_site |>
#'   sf::st_transform(4326) |>
#'   randomize_receivers(100, 500, TRUE)

randomize_receivers <- function(
  study_site,
  quantity = 100,
  receiver_range = 500,
  view
) {
  receivers_rand <- sf::st_sample(study_site, size = quantity)

  # Turn receivers into polygons
  receivers_rand <- sf::st_buffer(receivers_rand, dist = receiver_range)

  if (view == TRUE) {
    mapview::mapView(receivers_rand)
  }
}
