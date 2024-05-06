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
#' library(sporeg)
#' library(data.table)
#' res <- load(system.file("extdata", "res.Rda", package = "sporeg"))
#'
#' rcv_dens <- lapply(res, den_rcvs)
#' rcv_dens <- data.table::rbindlist(rcv_dens, idcol = 'resolution') %>%
#' left_join(tibble(resolution = 1:4,
#' res_name = c("100km", "50km", "25km", "10km")),
#' by = "resolution") %>%
#' mutate(res_name = ordered(res_name, levels = c("100km", "50km", "25km", "10km")))

den_rcvs <- function(df) {

  zero_up <- units::set_units(0, "1/km^2")

  den_rcv <- df %>%
    as.data.frame() %>%
    dplyr::filter(den_rcs > zero_up) %>%
    dplyr::summarise(dn_min = min(den_rcs),
              dn_mean = mean(den_rcs),
              dn_max = max(den_rcs),
              c_min = min(count),
              c_mean = mean(count),
              c_max = max(count))

  return(den_rcv)
}
