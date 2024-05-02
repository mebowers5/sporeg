#' Different cut off
#'
#' @param df data frame object consisting of results from iterative reconstruction process
#' @param depth_limit a depth value in meters that represents the new depth cut off of interest
#'
#' @return a data frame object containing the percentage of grid cells that contained a good fit out of all grid cells that had an average depth (mean_depth) less than or equal to the depth_limit
#' @export

dif_co <- function(df, depth_limit) {

  tot <- df %>%
    as.data.frame() %>%
    dplyr::filter(mean_depth <= depth_limit) %>%
    dplyr::count() %>%
    dplyr::rename(tot = n)

  df <- df %>%
    dplyr::filter(mean_depth <= depth_limit & g_fit == 1) %>%
    dplyr::count()

  df <- merge(tot, df) %>%
    dplyr::mutate(perc = n/tot*100)

  return(df)
}
