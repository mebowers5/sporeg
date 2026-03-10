FROM rocker/r2u

# https://specs.opencontainers.org/image-spec/annotations/
LABEL \
    org.opencontainers.image.authors="Beth Bowers <bowersme@si.edu>; Michael O'Brien <obrien@umces.edu>" \
    org.opencontainers.image.vendor="Smithsonian Environmental Research Center" \
    org.opencontainers.image.version="0.1.0-2" \
    org.opencontainers.image.source="https://github.com/mebowers5/sporeg" \
    org.opencontainers.image.licenses="MIT" \
    org.opencontainers.image.description="Ubuntu image with R and package dependencies needed to recreate the `sporeg` package workflow outlined in the R package vignette."

# Install binary from OTN trackyverse (https://ocean-tracking-network.r-universe.dev/sporeg)
#   Also installs (some) suggested packages via "dependencies = TRUE"
RUN Rscript -e "install.packages('sporeg', \
  repos = c('https://ocean-tracking-network.r-universe.dev'), \
  dependencies = TRUE)"

# Doesnt seem to install these packages for whatever reason
RUN Rscript -e "install.packages(c('mapview', 'oceanmap', 'car')); \
  remotes::install_github('nx10/httpgd')"

RUN echo 'options(httpgd.host = "0.0.0.0", httpgd.port = 8888)' >> /etc/R/Rprofile.site \
  && echo 'httpgd::hgd()' >> .Rprofile

EXPOSE 8888

ENTRYPOINT [ "R" ]
