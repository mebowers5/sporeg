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
#' randomize_receivers(sporeg::study_site |> sf::st_transform(., 4326), 100, 500, TRUE)
#'

randomize_receivers <- function(study_site, quantity, receiver_range, view) {

  quantity = size
  receiver_range = dist

  if(view == TRUE) {

receivers_rand <- st_sample(study_site, size = 100)

# Turn receivers into polygons
receivers_rand <- st_buffer(receivers_rand, dist = 500)

mapview::mapView(receivers_rand)
  }
  else {
    receivers_rand <- st_sample(study_site, size = 100)

    # Turn receivers into polygons
    receivers_rand <- st_buffer(receivers_rand, dist = 500)
  }
}
