#' Gaps characteristics
#'
#' @param df data frame object consisting of results from iterative reconstruction process
#'
#' @return a data frame object with summary statistics that provide insight into the locations of the gaps in the network receiver array that were closed by the reconstruction process
#' @export
#'
#' @examples clsd_gp_chars <- lapply(res, gps_chr) %>%
#'data.table::rbindlist(., idcol = 'resolution') %>%
#'   left_join(tibble(resolution = 1:4,
#'                    res_name = c("100km", "50km", "25km", "10km")),
#'             by = "resolution") %>%
#'             mutate(res_name = ordered(res_name, levels = c("100km", "50km", "25km", "10km")))

gps_chr <- function(df) {

  load(system.file("extdata", "res.Rda",
                   package = "sporeg"))

  df <- df %>%
    dplyr::filter(g_fit == 1 & p_a == 0) %>%
    dplyr::mutate(d_shore_km = d_shore/1000) %>%
    dplyr::summarise(max_dpth = max(na.omit(mean_depth)),
              min_dpth = min(na.omit(mean_depth)),
              avg_depth = mean(na.omit(mean_depth)),
              max_d = max(na.omit(d_shore_km)),
              min_d = min(na.omit(d_shore_km)),
              avg_d = mean(na.omit(d_shore_km)))

  return(df)
}
