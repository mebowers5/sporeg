#' Internal Legacy Argument Mapping Vector
#'
#' A named character vector used by internal helper functions to map
#' deprecated parameters to their modern replacements.
#' The names represent legacy arguments, and the values represent new names.
#'
#' @keywords internal
#' @name DEPRECATED_ARG_MAP
.DEPRECATED_ARG_MAP <- list(
  # from comp_trks
  "comp_trks" = c(
    "HSgrid" = "grid",
    "multi.grid" = "multi_grid",
    "sim_trks" = "simulated_tracks",
    "vis_graph" = "visibility_graph"
  ),

  # from cts
  "cts" = c("sg" = "grid", "df" = "rerouted_tracks"),

  # from d_shore_rcvs
  "d_shore_rcvs" = c("km" = "resolution", "sts_points" = "stations"),

  # from den_rcvs
  "den_rcvs" = c("df" = "gridded_receivers"),

  # from dif_co
  "dif_co" = c("df")
)


#' Check and Remap Deprecated Arguments from Dots
#'
#' @param dots A named list of arguments captured via `list(...)`.
#' @param mapping A named character vector. Defaults to the global master map.
#' @param env The environment of the calling function, defaulting to the parent
#'   frame. Mostly used for testing.
#'
#' @keywords internal
.check_deprecated_dots <- function(
  dots,
  mapping = .DEPRECATED_ARG_MAP,
  env = parent.frame()
) {
  # Find what function is calling this
  calling_function <- sys.call(-1)
  if (is.null(calling_function)) {
    return(invisible(NULL))
  }
  calling_function <- as.character(calling_function[[1]])[1]

  deprecated_args <- intersect(names(dots), names(mapping[[calling_function]]))

  for (old_arg in deprecated_args) {
    new_arg <- mapping[[old_arg]]

    warning(
      sprintf(
        "The argument '%s' is deprecated. Please use '%s' instead.",
        old_arg,
        new_arg
      ),
      call. = FALSE
    )

    new_arg_exists <- exists(new_arg, envir = env, inherits = FALSE)
    new_arg_val <- if (new_arg_exists) get(new_arg, envir = env) else NULL

    if (!new_arg_exists || is.null(new_arg_val)) {
      assign(new_arg, dots[[old_arg]], envir = env)
    }
  }
}


#' Deprecated functions
#'
#' These functions are provided for backwards compatibility and will be removed
#' in a future release.
#'
#' @rdname deprecated
#' @inheritParams compare_tracks
#' @export
comp_trks <- function(
  sim_trks,
  stations,
  land_barrier,
  vis_graph,
  HSgrid,
  multi.grid,
  snap_tolerance,
  cores = 2,
  ...
) {
  warning(c(
    "This function is deprecated and will be removed in future versions.\n",
    "Please use \"compare_tracks\" instead."
  ))
  .check_deprecated_dots(dots = list(...))

  compare_tracks(
    simulated_tracks,
    stations,
    land_barrier,
    visibility_graph,
    grid,
    multi_grid,
    snap_tolerance,
    cores = 2,
    ...
  )
}
