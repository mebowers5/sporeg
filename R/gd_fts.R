#' Good fits summary statistics
#'
#'This function summarizes minimum, maximum, and mean statistics for depth (mean_depth), distance from shore (d_shore), receiver count (count), receiver presence/absence (p_a), and the percentage of grid cells missing receivers for all of the good fits (g_fit).
#' @param df data frame object consisting of results from iterative reconstruction process
#'
#' @return a data frame object with summary statistics from grid cells that contained good fits
#' @export
#'
#' @examples
#' # Apply gd_fts to list of grid resolutions
#'
#' library(sporeg)
#' library(dplyr)
#' library(sf)
#' library(data.table)
#'
#' load(system.file("extdata", "res.Rda", package = "sporeg"))
#'
#' gd_fit_char <- lapply(res, gd_fts) %>%
#' data.table::rbindlist(., idcol = 'resolution') %>%
#'  left_join(tibble(resolution = 1:4,
#'                   res_name = c("100km", "50km", "25km", "10km")),
#'           by = "resolution") %>%
#'  dplyr::mutate(res_name = ordered(res_name, levels = c("100km", "50km", "25km", "10km")))

gd_fts <- function(df) {

  tot <- df %>%
    dplyr::filter(g_fit == 1) %>%
    dplyr::mutate(d_shore_km = d_shore/1000) %>%
    dplyr::summarise(max_dpth = max(na.omit(mean_depth)),
              min_dpth = min(na.omit(mean_depth)),
              avg_depth = mean(na.omit(mean_depth)),
              max_d = max(na.omit(d_shore_km)),
              min_d = min(na.omit(d_shore_km)),
              avg_d = mean(na.omit(d_shore_km)),
              max_ct = max(count),
              min_ct = min(count),
              avg_ct = mean(count),
              n_abs = sum(p_a == 0),
              tot = n(),
              perc_abs = n_abs/tot*100)

  return(tot)
}
