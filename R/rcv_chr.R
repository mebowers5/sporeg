#' Receiver characteristics
#'
#' @param df data frame object consisting of results from iterative reconstruction process
#'
#' @return a data frame object with summary statistics that provide insight into the locations of the receivers in the network receiver array
#' @export
#'
#' @examples rcv_chars <- lapply(res, rcv_chr) %>%
#' data.table::rbindlist(., idcol = 'resolution') %>%
#' left_join(tibble(resolution = 1:4,
#' res_name = c("100km", "50km", "25km", "10km")),
#'             by = "resolution") %>%
#'               mutate(res_name = ordered(res_name, levels = c("100km", "50km", "25km", "10km")))

rcv_chr <- function(df) {

  load(system.file("extdata", "res.Rda",
                   package = "sporeg"))

  tot <- df %>%
    as.data.frame() %>%
    dplyr::select(-x) %>%
    dplyr::filter(p_a == 1) %>%
    dplyr::mutate(d_shore_km = d_shore/1000) %>%
    dplyr::summarise(tot = n(),
              dpth_200_800 = sum(na.omit(mean_depth >= 200 & mean_depth <= 800)),
              depth_200_500 = sum(na.omit(mean_depth >= 200 & mean_depth <= 500)),
              min_dpth = min(na.omit(mean_depth)),
              avg_depth = mean(na.omit(mean_depth)),
              max_depth = max(na.omit(mean_depth)),
              max_d = max(na.omit(d_shore_km)),
              min_d = min(na.omit(d_shore_km)),
              avg_d = mean(na.omit(d_shore_km)))

  return(tot)
}
