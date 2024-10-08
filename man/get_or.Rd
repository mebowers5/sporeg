% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_or.R
\name{get_or}
\alias{get_or}
\title{Get odds ratios}
\usage{
get_or(model)
}
\arguments{
\item{model}{A model object created from the \code{nlme} package}
}
\value{
A data frame object with a point estimate and lower and upper 95\% confidence interval values associated with each covariate
}
\description{
This function calculates the odds ratios for each variable in your model.
}
\examples{
# Get the odds ratio for each variable in the final model

load(system.file("extdata", "fit2.100km.Rda", package = "sporeg"))

odds_100km <- get_or(fit2.100km) \%>\%
dplyr::mutate(res_name = "100km")
}
