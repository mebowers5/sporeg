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
#' den_rcvs(res[[1]])

den_rcvs <- function(df) {
  zero_up <- units::set_units(0, "1/km^2")

  den_rcv <- df |>
    # Redundant, but needed to avoid a vctrs error
    sf::st_as_sf() |>
    as.data.frame() |>
    dplyr::filter(den_rcs > zero_up) |>
    dplyr::summarise(
      dn_min = min(den_rcs),
      dn_mean = mean(den_rcs),
      dn_max = max(den_rcs),
      c_min = min(count),
      c_mean = mean(count),
      c_max = max(count)
    )

  return(den_rcv)
}
