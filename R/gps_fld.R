#' Gaps filled
#'
#'This function calculates how well the reconstructions closed gaps in the network receiver array
#' @param df data frame object consisting of results from iterative reconstruction process
#'
#' @return a data frame object with a percentage of grid cells that contained a good fit out of those that lacked receivers
#' @export
#'
#' @examples
#' # Apply gps_fld to list of grid resolutions
#'
#' library(sporeg)
#' library(dplyr)
#' library(sf)
#' library(data.table)
#' load(system.file("extdata", "res.Rda", package = "sporeg"))
#'
#' clsd_gps <- lapply(res, gps_fld) %>%
#' data.table::rbindlist(., idcol = 'resolution') %>%
#'  left_join(tibble(resolution = 1:4,
#'                    res_name = c("100km", "50km", "25km", "10km")),
#'            by = "resolution") %>%
#'  dplyr::mutate(res_name = ordered(res_name, levels = c("100km", "50km", "25km", "10km")))

gps_fld <- function(df) {

  tot <- df %>%
    dplyr::filter(p_a == 0) %>%
    dplyr::count() %>%
    dplyr::rename(tot = n)

  df <- df %>%
    as.data.frame() %>%
    dplyr::select(-x) %>%
    dplyr::filter(g_fit == 1 & p_a == 0) %>%
    dplyr::count() %>%
    merge(., tot)

  perc <- df %>%
    dplyr::mutate(perc = n/tot*100)

  return(perc)
}
