#' Counts
#'
#' The function allows you to calculate counts per grid cell in demonstrative modeled movement data.
#' @param sg spatial grid created from `grid_res` function
#' @param df sf object of reconstructed, re-routed, optionally buffered tracks created from `sub_rrt` function
#'
#' @return A simple feature object with counts associated with grid cell IDs `"gid"`
#' @export

cts <- function(sg, df) {

  gid <- seq(1:max(sg$gid))  #Create a range of every grid cell
  all <- as.data.frame(gid)

  counts <- sf::st_join(sg, df, join = sf::st_intersects) %>%
    dplyr::distinct(gid, ID, geometry)

  counts <- aggregate(ID ~ gid, data = counts, FUN = length) %>%
    dplyr::rename(count = ID)

  counts <- dplyr::left_join(all, counts, by = "gid") %>%
    dplyr::mutate_all(~replace(., is.na(.), 0))  # Replace NA's with zeros so we can check count

  counts <- merge(counts, sg)

  return(counts)
}
