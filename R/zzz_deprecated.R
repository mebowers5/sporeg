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
  "d_shore_rcvs" = c(
    "km" = "resolution",
    "sts_points" = "stations",
    "epsg" = "crs"
  ),

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
#' @param sim_trks Tracks to which modeled tracks should be compared
#' @param vis_graph A visibility graph created from the barrier object @seealso [pathroutr::prt_visgraph]
#' @param HSgrid When multi_grid = FALSE, a sf polygon grid; When multi_grid = TRUE, a list of sf polygon grids
#' @param multi.grid boolean
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

#' Grid counts
#'
#' The function allows you to calculate counts per grid cell in demonstrative modeled movement data.
#' @param sg spatial grid created from `grid_res` function
#' @param df sf object of reconstructed, re-routed, optionally buffered tracks created from `sub_rrt` function
#'
#' @return A simple feature object with counts associated with grid cell IDs `"gid"`
#' @rdname deprecated
#' @export
cts <- function(sg, df) {
  warning(c(
    "This function is deprecated and will be removed in future versions.\n",
    "Please use \"grid_counts\" instead."
  ))
  .check_deprecated_dots(dots = list(...))

  grid_counts(grid, rerouted_tracks)
}


#' Distance to shore and density of receivers
#'
#' This function determines the distance to shore and the density of receivers for each grid cell.
#' @param km grid cell resolution in km. one-sided length of grid cell, assumes desired grid cell is to be squared
#' @param sts_pts a simple feature (multi)point object representing receiver locations with detection range buffer
#' @param epsg coordinate reference system specification for transformation
#'
#' @return A simple feature multipolygon object with information on distance to shore from grid cell center, receiver presence/absence, counts, and densities
#' @rdname deprecated
#' @inheritParams distance_to_shore
#' @export
d_shore_rcvs <- function(km, study_site, land_barrier, epsg, sts_pts) {
  warning(c(
    "This function is deprecated and will be removed in future versions.\n",
    "Please use \"distance_to_shore\" instead."
  ))
  .check_deprecated_dots(dots = list(...))

  distance_to_shore(
    resolution,
    study_site,
    land_barrier,
    epsg,
    stations
  )
}
