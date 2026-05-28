#' Different cut off
#'
#' @param reconstructed data frame object consisting of results from iterative reconstruction process.
#'   Needs columns `mean_depth` and `g_fit`; i.e., needs to have been passed through
#'   `get_depth` and `gd_fts`.
#' @param depth_limit a depth value in meters that represents the new depth cut off of interest
#'
#' @return a data frame object containing the percentage of grid cells that contained
#'   a good fit out of all grid cells that had an average depth (mean_depth) less
#'   than or equal to the depth_limit
#' @export
#'
#' @examples
#' # Apply depth_cutoff to list of grid resolutions
#'
#' dif_depth <- lapply(res, depth_cutoff, depth_limit = 300) |>
#'   dplyr::bind_rows(.id = 'resolution') |>
#'   dplyr::mutate(resolution = as.numeric(resolution)) |>
#'   dplyr::left_join(dplyr::tibble(resolution = 1:4,
#'     res_name = c("100km", "50km", "25km", "10km")),
#'     by = "resolution") |>
#'   dplyr::mutate(
#'     res_name = ordered(res_name, levels = c("100km", "50km", "25km", "10km"))
#'   )

depth_cutoff <- function(reconstructed, depth_limit) {
  tot <- reconstructed |>
    as.data.frame() |>
    dplyr::filter(mean_depth <= depth_limit) |>
    dplyr::count() |>
    dplyr::rename(tot = n)

  reconstructed <- reconstructed |>
    dplyr::filter(mean_depth <= depth_limit & g_fit == 1) |>
    dplyr::count()

  reconstructed <- merge(tot, reconstructed) |>
    dplyr::mutate(perc = n / tot * 100)

  return(reconstructed)
}
