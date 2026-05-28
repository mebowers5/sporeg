#' Run a power analysis
#'
#' This is a wrapper function for running a power analysis on iterative simulation
#'   and reconstruction methods process.
#'
#' @param results the resulting data frame from the iterative methods process
#' @param significance_level numeric. the desired level of significance to achieve
#' @param power the desired level of power to achieve
#' @param delta numeric. the desired effect size to achieve
#' @param n integer. the number of replicates/sample size. should be assigned
#'   `NULL` if desiring sample size
#'
#' @return A data frame object
#' @export
#'
#' @examples
#' # Use power_analysis wrapper function on example results
#' # Grab 100km resolution
#' res <- lapply(sporeg::results, `[[`, 1) |>
#'   dplyr::bind_rows() |>
#'   # Calculate standard deviation by group
#'   dplyr::group_by(gid) |>
#'   dplyr::summarise(sd = sd(dif)) |>
#'   dplyr::ungroup()
#'
#' anims <- 30
#' power <- 0.8
#' delta <- anims * 0.01 # a delta within 1% of the total number of animals
#' sig.level <- 0.95
#'
#' power_analysis(res, sig.level, power, delta)

power_analysis <- function(
  results,
  significance_level,
  power,
  delta,
  n = NULL
) {
  stats::power.t.test(
    n = n,
    sd = max(na.omit(results$sd)),
    sig.level = significance_level,
    power = power,
    delta = delta,
    type = "paired",
    alternative = "two.sided"
  )
}
