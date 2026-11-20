#' @export
impute_track.sporeg_crawl <- function(model, data, ...) {
  data_crs <- sf::st_crs(data)
  data <- data |>
    sf::st_drop_geometry()

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
    dplyr::select(ID, uid, x = mu.x, y = mu.y) |>
    sf::st_as_sf(coords = c("x", "y"), crs = data_crs)
}


#' Configure arguments for `aniMotum::fit_ssm()`
#'
#' @param model State-space model to fit (`"crw"` or `"rw"`).
#' @param ... Additional arguments passed to [aniMotum::fit_ssm()].
#'
#' @return An object of class `"sporeg_animotum_ssm"`.
#' @export
animotum_ssm <- function(model = "crw", ...) {
  structure(
    c(list(model = model), list(...)),
    class = "sporeg_animotum_ssm"
  )
}

#' @export
impute_track.sporeg_animotum_ssm <- function(model, data, ...) {
  if (!requireNamespace("aniMotum", quietly = TRUE)) {
    stop(
      "'aniMotum' must be installed to use `model = 'animotum_ssm'`.",
      call. = FALSE
    )
  }

  m_tracks <- data |>
    dplyr::transmute(
      id = ID,
      date = as.POSIXct(uid * 3600, tz = "UTC"),
      lc = "G",
      geometry = mean_position
    )

  mandatory_arguments <- list(
    x = m_tracks
  )
  default_arguments <- list(
    model = "crw",
    time.step = 1
  )

  ssm_arguments <- utils::modifyList(default_arguments, unclass(model))
  ssm_arguments <- utils::modifyList(ssm_arguments, list(...))
  ssm_arguments <- utils::modifyList(ssm_arguments, mandatory_arguments)

  out <- do.call(aniMotum::fit_ssm, ssm_arguments)

  aniMotum::grab(out, "predicted", as_sf = T) |>
    sf::st_transform(sf::st_crs(data)) |> # temporary fix for issue #93 in aniMotum
    dplyr::transmute(
      ID = id,
      uid = as.numeric(date) / 3600
    )
}
