#' Summarize receiver density and counts
#'
#' This function provides summary statistics on the densities and counts of receivers in a grid.
#' @param df A simple feature polygon object (a grid)
#'
#' @return A data frame object with minimum, mean, and maximum of receiver densities and counts in km^-2
#' @export
#'
#' @examples
#'
#' receiver_density(resolutions[[1]])

receiver_density <- function(df) {
  zero_up <- units::set_units(0, "1/km^2")

  rec_density <- df |>
    # Redundant, but needed to avoid a vctrs error
    sf::st_as_sf() |>
    as.data.frame() |>
    dplyr::filter(den_rcs > zero_up) |>
    dplyr::summarise(
      density_min = min(den_rcs),
      density_mean = mean(den_rcs),
      density_max = max(den_rcs),
      count_min = min(count),
      count_mean = mean(count),
      count_max = max(count)
    )

  return(rec_density)
}
