#' Run a power analysis
#'
#' This is a wrapper function for running a power analysis on iterative simulation
#'   and reconstruction methods process.
#'
#' @param output the resulting data frame from the iterative methods process
#' @param sig.level numeric. the desired level of significance to achieve
#' @param power numeric. the desired level of power to achieve
#' @param delta numeric. the desired effect size to achieve
#' @param n integer. the number of replicates/sample size. should be assigned
#'   `NULL` if desiring sample size
#'
#' @return A data frame object
#' @export
#'
#' @examples
#' # Use powr wrapper function on example results
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
#' powr(res, sig.level, power, delta)

powr <- function(output, sig.level, power, delta, n = NULL) {
  stats::power.t.test(
    n = n,
    sd = max(na.omit(output$sd)),
    sig.level = sig.level,
    power = power,
    delta = delta,
    type = "paired",
    alternative = "two.sided"
  )
}
