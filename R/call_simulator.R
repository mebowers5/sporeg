#' Simulate movement tracks
#'
#' @param model A call_simulator specification object (e.g., [crw_in_polygon()]),
#'   a character string, or a custom simulator function.
#' @param ... Additional arguments passed to specific simulator methods.
#'
#' @return A list of simulated animal tracks (typically `sf` objects).
#' @export
call_simulator <- function(model, ...) {
  UseMethod("call_simulator")
}

#' @describeIn call_simulator Execute a custom simulated track.
#' @export
call_simulator.function <- function(model, ...) {
  model(...)
}


#' Configure arguments for `glatos::crw_in_polygon()`
#'
#' @param ... Default arguments forwarded to [glatos::crw_in_polygon()] (e.g.,
#'   `theta`, `initHeading`, `show_progress`).
#'
#' @return An object of class `"sporeg_crw_in_polygon"`.
#' @export
crw_in_polygon <- function(...) {
  structure(list(...), class = "sporeg_crw_in_polygon")
}

#' Simulate tracks as a correlated random walk inside a polygon using
#'   [glatos::crw_in_polygon()]
#'
#' @param anims Integer. Number of individuals to simulate.
#' @param study_area An `sf` polygon defining the allowed simulation boundary.
#' @param n_days Integer. Number of days to simulate (at 1-hour steps).
#' @param initial_positions Optional starting `(x, y)` coordinates.
#'   Defaults to `NULL` (sampled randomly within `study_area`).
#' @param ... Additional arguments passed to [glatos::crw_in_polygon()]. The
#'   following are required:
#'   * `vmin`: Numeric scalar. Minimum velocity in meters per second from which
#'     to sample step length.
#'   * `vmax`: Numeric scalar. Maximum velocity in meters per second from which
#'     to sample step length.
#'
#' @export
call_simulator.sporeg_crw_in_polygon <- function(
  model,
  anims,
  study_area,
  n_days,
  initial_positions = NULL,
  ...
) {
  if (is.null(initial_positions)) {
    initial_positions <- matrix(c(NA, NA), nrow = anims, ncol = 2)
  }
  dots <- list(...)
  vmin <- dots$vmin
  vmax <- dots$vmax
  dots[c("vmin", "vmax")] <- NULL

  # Sample between minimum and maximum velocity to set step length per hour for each individual
  step_length <- stats::runif(anims, vmin, vmax) * 60 * 60

  lapply(seq_len(anims), function(i) {
    crw_arguments <- build_arguments(
      model,
      mandatory = list(
        polyg = study_area,
        stepLen = step_length[i],
        initPos = initial_positions[i, 1:2],
        nsteps = n_days * 24
      ),
      dots = dots
    )

    do.call(glatos::crw_in_polygon, crw_arguments)
  })
}

# Note, these currently do not work.

#' @export
animotum_sim <- function(model = "crw", ...) {
  structure(c(list(model = model), list(...)), class = "sporeg_animotum_sim")
}

#' @export
call_simulator.sporeg_animotum_sim <- function(
  model,
  anims,
  study_area = NULL,
  n_days,
  initial_positions = NULL,
  ...
) {
  if (!requireNamespace("aniMotum", quietly = TRUE)) {
    stop(
      "'aniMotum' must be installed to use `model = 'animotum_ssm'`.",
      call. = FALSE
    )
  }

  # needs input in km
  initial_positions <- initial_positions / 1000

  lapply(seq_len(anims), function(i) {
    sim_arguments <- build_arguments(
      model,
      mandatory = list(
        N = n_days * 24,
        start = list(
          initial_positions[i, 1:2],
          as.POSIXct(0, tz = "UTC")
        ),
        ts = 1
      ),
      dots = list(...)
    )

    do.call(aniMotum::sim, sim_arguments)
  })
}
