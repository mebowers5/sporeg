#' Summarize receiver density and counts
#'
#' This function provides summary statistics on the densities and counts of receivers in a grid.
#' @param gridded_receivers A simple feature polygon object (a grid) which has columns of `den_rcs` and `count`.
#'   This is usually the result of `res_grid` which has been passed through `distance_to_shore`;
#'   i.e.: `res_grid() |> d_shore_rcvs()`.
#'
#' @return A data frame object with minimum, mean, and maximum of receiver densities and counts in km^-2
#' @export
#'
#' @examples
#'
#' receiver_density(res[[1]])

receiver_density <- function(gridded_receivers) {
  zero_up <- units::set_units(0, "1/km^2")

  den_rcv <- gridded_receivers |>
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
