#' Gaps filled
#'
#' This function calculates how well the reconstructions closed gaps in the network
#'   receiver array
#' @param df data frame object consisting of results from iterative reconstruction process
#'
#' @return a data frame object with a percentage of grid cells that contained a
#'   good fit out of those that lacked receivers
#' @export
#'
#' @examples
#' # Apply gaps_filled to list of grid resolutions
#'
#' closd_gaps <- lapply(resolutions, gaps_filled) |>
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

gaps_filled <- function(df) {
  perc <- df |>
    dplyr::filter(p_a == 0) |>
    dplyr::summarize(
      n = sum(g_fit == 1, na.rm = TRUE),
      tot = dplyr::n(),
      perc = n / tot * 100
    )

  return(perc)
}
