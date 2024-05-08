#' Run a power analysis
#'
#' This is a wrapper function for running a power analysis on iterative simulation and reconstruction methods process.
#' @param output the resulting data frame from the iterative methods process
#' @param sig.level numeric. the desired level of significance to achieve
#' @param power numeric. the desired level of power to achieve
#' @param delta numeric. the desired effect size to achieve
#' @param n integer. the number of replicates/sample size. should be assigned `NULL` if desiring sample size
#'
#' @return A data frame object
#' @export
#'
#' @examples
#' # Use powr wrapper function on example results
#'
#' library(sporeg)
#' library(dplyr)
#' library(data.table)
#' load(system.file("extdata", results.Rda, package = "sporeg"))
#'
#' results <- lapply(results, data.table::rbindlist, idcol = 'resolution')
#' results <- data.table::rbindlist(results, idcol = 'iteration') %>%
#' left_join(tibble(resolution = 1:4,
#' res_name = c("100km", "50km", "25km", "10km")),
#' by = "resolution")
#'
#' df_var <- zero_var(results)
#' pow_stat <- df_var %>%
#' dplyr::group_by(res_name, gid) %>%
#' dplyr::summarise(sd = sd(dif)) %>%
#' dplyr::ungroup() %>%
#' dplyr::group_by(res_name)
#'
#' res <- pow_stat %>% dplyr::filter(res_name == "100km")
#' pwr_100km <- powr(res, sig.level, power, delta)
#' print("Grid cell resolution: 100 km x 100 km")
#' pwr_100km

powr <- function(output, sig.level, power, delta, n) {
  stats::power.t.test(n = NULL, sd = max(na.omit(output$sd)),
                      sig.level = sig.level, power = power, delta = delta,
                      type = "paired", alternative = "two.sided")
}
