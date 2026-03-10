#' Summarize receiver density and counts
#'
#' This function provides summary statistics on the densities and counts of receivers in a grid.
#' @param df A simple feature polygon object (a grid)
#'
#' @return A data frame object with minimum, mean, and maximum of receiver densities and counts in km^-2
#' @export
#'
#' @examples
#' # Apply den_rcvs to list of grid resolutions
#'
#' rcv_dens <- lapply(res, den_rcvs)
#'
#' rcv_dens <- dplyr::bind_rows(rcv_dens, .id = 'resolution') |>
#'  dplyr::mutate(resolution = as.numeric(resolution)) |>
#'  dplyr::left_join(
#'    dplyr::tibble(
#'     resolution = 1:4,
#'     res_name = c("100km", "50km", "25km", "10km")
#'    ),
#'    by = "resolution") |>
#' dplyr::mutate(res_name = ordered(res_name, levels = c("100km", "50km", "25km", "10km")))

den_rcvs <- function(df) {
  zero_up <- units::set_units(0, "1/km^2")

  den_rcv <- df |>
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
