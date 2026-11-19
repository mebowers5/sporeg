#' @export
impute_track.sporeg_crawl <- function(model, data, ...) {
  mandatory_arguments <- list(
    obsData = data,
    Time.name = "uid"
  )
  default_arguments <- list(
    timeStep = 0.25,
    fixPar = c(NA, NA),
    theta = c(8, 0),
    attempts = 100,
    retrySD = 5,
    retryFits = 5,
    ncores = getOption("mc.cores", default = 2),
    retryParallel = TRUE
  )

  # Let dots modify the default arguments
  crawl_arguments <- utils::modifyList(default_arguments, list(...))
  # Add in mandatory arguments
  crawl_arguments <- utils::modifyList(crawl_arguments, mandatory_arguments)

  crawl_out <- do.call(momentuHMM::crawlWrap, crawl_arguments)

  data.frame(crawl_out$crwPredict) |>
    dplyr::select(ID, uid, x = mu.x, y = mu.y)
}
