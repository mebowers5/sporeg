#' Gaps characteristics
#'
#' @param df data frame object consisting of results from iterative reconstruction process
#'
#' @return a data frame object with summary statistics that provide insight into
#'   the locations of the gaps in the network receiver array that were closed by
#'   the reconstruction process
#'
#' @export
#'
#' @examples
#' # Apply gaps_info to list of grid resolutions
#'
#' closed_gaps_info <- lapply(resolutions, gaps_info) |>
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

gaps_info <- function(df) {
  df <- df |>
    dplyr::filter(g_fit == 1 & p_a == 0)

  if (nrow(df) == 0) {
    warning("No gaps found.")
    hold <- data.frame(
      max_depth = numeric(),
      min_depth = numeric(),
      avg_depth = numeric(),
      max_d = numeric(),
      min_d = numeric(),
      avg_d = numeric()
    )

    geom_col <- attr(df, "sf_column")
    hold[[geom_col]] <- sf::st_geometry(df)[0]
    hold <- sf::st_sf(hold, sf_column_name = geom_col)

    return(hold)
  } else {
    df <- df |>
      dplyr::mutate(d_shore_km = d_shore / 1000) |>
      dplyr::summarise(
        max_depth = max(mean_depth, na.rm = T),
        min_depth = min(mean_depth, na.rm = T),
        avg_depth = mean(mean_depth, na.rm = T),
        max_d = max(d_shore_km, na.rm = T),
        min_d = min(d_shore_km, na.rm = T),
        avg_d = mean(d_shore_km, na.rm = T)
      )

    return(df)
  }
}
