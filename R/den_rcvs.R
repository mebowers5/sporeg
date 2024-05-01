#' Summarize receiver density and counts
#'
#' This function provides summary statistics on the densities and counts of receivers in a grid.
#' @param df A simple feature polygon object (a grid)
#'
#' @return A data frame object with minimum, mean, and maximum of receiver densities and counts in km^-2
#' @export

den_rcvs <- function(df) {

  den_rcs <- NULL

  den_rcv <- df %>%
    as.data.frame() %>%
    dplyr::filter(den_rcs > 0) %>%
    dplyr::summarise(dn_min = min(den_rcs),
              dn_mean = mean(den_rcs),
              dn_max = max(den_rcs),
              c_min = min(count),
              c_mean = mean(count),
              c_max = max(count))

  return(den_rcv)
}
