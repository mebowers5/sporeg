#' Summarize receiver density and counts
#' Summarize the density and counts of receivers in a grid
#'
#' @param df A simple feature polygon object (a grid)
#'
#' @return A data frame object with minimum, mean, and maximum of receiver densities and counts in km^-2
#' @export
#'
#' @examples
#' res <- list(HS_100km_grid, HS_50km_grid, HS_25km_grid, HS_10km_grid)
#' rcv_dens <- lapply(res, den_rcvs)
#' rcv_dens <- data.table::rbindlist(rcv_dens, idcol = 'resolution') %>%
#' left_join(tibble(resolution = 1:4, res_name = c("100km", "50km", "25km", "10km")), by = "resolution") %>%
#' mutate(res_name = ordered(res_name, levels = c("100km", "50km", "25km", "10km")))
den_rcvs <- function(df) {
  
  den_rcv <- df %>%
    as.data.frame() %>%
    dplyr::select(-x) %>%
    dplyr::filter(den_rcs > units::set_units(0, 1/km^2)) %>%
    summarise(dn_min = min(den_rcs),
              dn_mean = mean(den_rcs),
              dn_max = max(den_rcs),
              c_min = min(count),
              c_mean = mean(count),
              c_max = max(count))
  
  return(den_rcv)
}