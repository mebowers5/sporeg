#' Impute missing telemetry locations using a movement model
#'
#' @param model A model specification object, character string (e.g., `"crawl"`),
#'   or a custom function `function(data, ...)`.
#' @param data A data.frame with columns `ID`, `uid`, `x`, and `y`.
#' @param ... Additional arguments passed to the underlying model method.
#'
#' @return A data.frame with `ID`, and `uid`, and imputed values of `x` and `y`.
#' @export
impute_track <- function(model, data, ...) {
  UseMethod("impute_track")
}

#' @export
impute_track.function <- function(model, data, ...) {
  model(data = data, ...)
}
