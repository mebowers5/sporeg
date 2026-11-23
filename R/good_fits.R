#' Good fits summary statistics
#'
#'This function summarizes minimum, maximum, and mean statistics for depth (mean_depth), distance from shore (d_shore), receiver count (count), receiver presence/absence (p_a), and the percentage of grid cells missing receivers for all of the good fits (g_fit).
#' @param df data frame object consisting of results from iterative reconstruction process
#'
#' @return a data frame object with summary statistics from grid cells that contained good fits
#' @export
#'
#' @examples
#' # Apply good_fits to list of grid resolutions
#'
#' good_fit_info <- lapply(resolutions, good_fits) |>
#'   dplyr::bind_rows(.id = 'resolution') |>
#'   dplyr::mutate(resolution = as.numeric(resolution)) |>
#'   dplyr::left_join(dplyr::tibble(resolution = 1:4,
#'     res_name = c("100km", "50km", "25km", "10km")),
#'     by = "resolution") |>
#'   dplyr::mutate(
#'     res_name = ordered(res_name, levels = c("100km", "50km", "25km", "10km"))
#'   )
good_fits <- function(df) {
  tot <- df |>
    dplyr::filter(g_fit == 1)

  if (nrow(tot) == 0) {
    warning("No good fits found.")
    hold <- data.frame(
      max_depth = numeric(),
      min_depth = numeric(),
      avg_depth = numeric(),
      max_d = numeric(),
      min_d = numeric(),
      avg_d = numeric(),
      max_ct = integer(),
      min_ct = integer(),
      avg_ct = numeric(),
      n_abs = integer(),
      tot = integer(),
      perc_abs = numeric()
    )

    geom_col <- attr(df, "sf_column")
    hold[[geom_col]] <- sf::st_geometry(df)[0]
    hold <- sf::st_sf(hold, sf_column_name = geom_col)

    return(hold)
  } else {
    tot <- tot |>
      dplyr::mutate(d_shore_km = d_shore / 1000) |>
      dplyr::summarise(
        max_depth = max(mean_depth, na.rm = T),
        min_depth = min(mean_depth, na.rm = T),
        avg_depth = mean(mean_depth, na.rm = T),
        max_d = max(d_shore_km, na.rm = T),
        min_d = min(d_shore_km, na.rm = T),
        avg_d = mean(d_shore_km, na.rm = T),
        max_ct = max(count),
        min_ct = min(count),
        avg_ct = mean(count),
        n_abs = sum(p_a == 0),
        tot = dplyr::n(),
        perc_abs = n_abs / tot * 100
      )

    return(tot)
  }
}
