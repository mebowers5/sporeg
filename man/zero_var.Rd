% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zero_var.R
\name{zero_var}
\alias{zero_var}
\title{Remove zero variance}
\usage{
zero_var(df)
}
\arguments{
\item{df}{data frame object of iterative methods results}
}
\value{
a data frame object with grid IDs that did not exhibit zero variance
}
\description{
This function removes grid cells that lacked any variance.
}
\examples{
# Remove grid cells with zero variance
library(sporeg)
library(dplyr)
load(system.file("extdata", "results.Rda", package = "sporeg"))

results <- lapply(results, data.table::rbindlist, idcol = 'resolution')
results <- data.table::rbindlist(results, idcol = 'iteration')
results <- results \%>\%
left_join(tibble(resolution = 1:4,
res_name = c("100km", "50km", "25km", "10km")),
by = "resolution")

df_var <- zero_var(results)
}
