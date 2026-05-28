#' Gaps filled
#'
#' This function calculates how well the reconstructions closed gaps in the network
#'   receiver array
#' @param reconstructed data frame object consisting of results from iterative reconstruction process
#'
#' @return a data frame object with a percentage of grid cells that contained a
#'   good fit out of those that lacked receivers
#' @export
#'
#' @examples
#' # Apply gaps_filled to list of grid resolutions
#'
#' clsd_gps <- lapply(res, gaps_filled) |>
#'   dplyr::bind_rows(.id = 'resolution') |>
#'     dplyr::left_join(
#'       data.frame(
#'         resolution = as.character(1:4),
#'         res_name = c("100km", "50km", "25km", "10km")
#'       ),
#'       by = "resolution") |>
#'   dplyr::mutate(
#'     res_name = ordered(res_name, levels = c("100km", "50km", "25km", "10km"))
#'   )

gaps_filled <- function(reconstructed) {
  tot <- reconstructed |>
    dplyr::filter(p_a == 0) |>
    dplyr::count() |>
    dplyr::rename(tot = n)

  reconstructed <- reconstructed |>
    as.data.frame() |>
    dplyr::select(-x) |>
    dplyr::filter(g_fit == 1 & p_a == 0) |>
    dplyr::count() |>
    merge(tot)

  perc <- reconstructed |>
    dplyr::mutate(perc = n / tot * 100)

  return(perc)
}
