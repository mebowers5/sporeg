#' Get odds ratios
#'
#' This function calculates the odds ratios for each variable in your model.
#' @param model A model object created from the `nlme` package
#'
#' @return A data frame object with a point estimate and lower and upper 95% confidence interval values associated with each covariate
#' @export
#'
#' @examples # Get the odds ratio for each variable in the final model
#'
#' load(system.file("extdata", "fit2.100km.Rda", package = "sporeg"))
#'
#' odds_100km <- get_or(fit2.100km) %>%
#' dplyr::mutate(res_name = "100km")

get_or <- function(model) {
  coefs <- as.data.frame(summary(model)$coefficients)
  row <- row.names(coefs) %in% c('(Intercept)', 'p_a1', 'mean_depth', 'den_rcs', 'd_shore')
  lower <- coefs[row,'Estimate'] - 1.96*coefs[row, 'Std. Error']
  upper <- coefs[row,'Estimate'] + 1.96*coefs[row, 'Std. Error']

  edf <- coefs %>%
    tibble::rownames_to_column(var = "Effect") %>%
    dplyr::select(Effect)

  or <- cbind(edf, exp(cbind(Odds_Ratio = coefs[row, 'Estimate'], low_CI_95 = lower, hi_CI_95 = upper)))

  return(or)
}
