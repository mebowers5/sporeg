# sporeg

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10198495.svg)](https://doi.org/10.5281/zenodo.10198495)

<!-- badges: end -->

The goal of sporeg is to reconstruct passive telemetry tracks using movement models and measure the accuracy of the reconstructions. This provides information on the reliability of ecological conclusions such as population distribution.

## Installation

Sporeg requires R version >= 4.3.2. The latest version of sporeg can be installed from GitHub using the following:

To install sporeg, first install the remotes package and some dependencies that live on GitHub

``` r
install.packages("remotes")
library(remotes)
remotes::install_github('mebowers5/sporeg', build_vignettes = TRUE, dependencies = TRUE)
```

If you are having trouble installing sporeg, try installing glatos and pathroutr first:

``` r
remotes::install_github("ocean-tracking-network/glatos")
remotes::install_github("jmlondon/pathroutr") 
```

## Step-by-step guide

The vignette(s) for this package can be accessed via the following code:

``` r
browseVignettes(package = "sporeg")
```

## Have a suggestion? Found a bug?

For any suggestions or problems, please create a new issue on the sporeg GitHub page (<https://github.com/mebowers5/sporeg/issues>).

You don't have any experience documenting issues on GitHub? No problem! Start here: <https://docs.github.com/en/issues/tracking-your-work-with-issues/quickstart>.


## Acknowledgements
The creation of this package was made possible by the helpful, supportive hosts and attendees of Ocean Tracking Network Study Hall, particularly Mike O'Brien, Ben Hlina, Jon Pye, and Sarah Popova. In addition, I thank Josh London for help with pathroutr code and encouragement. I also thank Brian J. Smith for introducing me to the world of movement modeling and for all his help and encouragement early on.

