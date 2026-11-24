#' Impute missing telemetry locations using a movement model
#'
#' @param model A model specification object, character string (e.g., `"crawl"`),
#'   or a custom function `function(data, ...)`.
#' @param data A data.frame with columns `ID`, `uid`, `x`, and `y`.
#' @param ... Additional arguments passed to the underlying model method.
#'
#' @return A data.frame with `ID`, and `uid`, and imputed values of `x` and `y`.
#' @export
call_imputor <- function(model, data, ...) {
  UseMethod("call_imputor")
}

#' @describeIn call_imputor Execute a custom imputation function.
#' @export
call_imputor.function <- function(model, data, ...) {
  model(data = data, ...)
}


#' Configure arguments for `momentuHMM::crawlWrap()`
#'
#' @param ... Default arguments forwarded to [momentuHMM::crawlWrap()] (e.g.,
#'   `timeStep`, `fixPar`, `theta`, `attempts`).
#'
#' @return An object of class `"sporeg_crawl"`.
#' @export
crawl <- function(...) {
  structure(list(...), class = "sporeg_crawl")
}

#' @describeIn call_imputor Impute tracks using [momentuHMM::crawlWrap()].
#' @export
call_imputor.sporeg_crawl <- function(model, data, ...) {
  data_crs <- sf::st_crs(data)
  data <- data |>
    sf::st_drop_geometry()

  crawl_arguments <- build_arguments(
    model,
    mandatory = list(
      obsData = data,
      Time.name = "uid"
    ),
    default = list(
      timeStep = 0.25,
      fixPar = c(NA, NA),
      theta = c(8, 0),
      attempts = 100,
      retrySD = 5,
      retryFits = 5,
      ncores = getOption("mc.cores", default = 2),
      retryParallel = TRUE
    ),
    dots = list(...)
  )

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

#' @describeIn call_imputor Impute tracks using [aniMotum::fit_ssm()].
#' @export
call_imputor.sporeg_animotum_ssm <- function(model, data, ...) {
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

  ssm_arguments <- build_arguments(
    model,
    mandatory = list(x = m_tracks),
    default = list(model = "crw", time.step = 1),
    dots = list(...)
  )

  out <- do.call(aniMotum::fit_ssm, ssm_arguments)

  aniMotum::grab(out, "predicted", as_sf = T) |>
    sf::st_transform(sf::st_crs(data)) |> # temporary fix for issue #93 in aniMotum
    dplyr::transmute(
      ID = id,
      uid = as.numeric(date) / 3600
    )
}
