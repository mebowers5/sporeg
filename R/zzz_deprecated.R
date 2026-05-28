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
  "dif_co" = c("df" = "reconstructed"),

  # from get_depth
  "get_depth" = c("HSgrid" = "grid"),

  # from gps_chr
  "gps_chr" = c("df" = "reconstructed")
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

#' Utility function to warn about function deprecation
#' @param old_name old function name
#' @param new_name new function name
#' @keywords internal
warn_deprecated <- function(old_name, new_name) {
  warning(c(
    old_name,
    " is deprecated and will be removed in future versions.\n",
    "Please use ",
    new_name,
    " instead."
  ))
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
  warn_deprecated("comp_trks", "compare_tracks")
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

#' @param sg spatial grid created from `grid_res` function
#' @param df sf object of reconstructed, re-routed, optionally buffered tracks created from `sub_rrt` function
#'
#' @rdname deprecated
#' @export
cts <- function(sg, df) {
  warn_deprecated("cts", "grid_counts")
  .check_deprecated_dots(dots = list(...))

  grid_counts(grid, rerouted_tracks)
}


#' @param km grid cell resolution in km. one-sided length of grid cell, assumes desired grid cell is to be squared
#' @param sts_pts a simple feature (multi)point object representing receiver locations with detection range buffer
#' @param epsg coordinate reference system specification for transformation
#'
#' @rdname deprecated
#' @inheritParams distance_to_shore
#' @export
d_shore_rcvs <- function(km, study_site, land_barrier, epsg, sts_pts) {
  warn_deprecated("d_shore_rcvs", "distance_to_shore")
  .check_deprecated_dots(dots = list(...))

  distance_to_shore(
    resolution,
    study_site,
    land_barrier,
    epsg,
    stations
  )
}

#' @param df A simple feature polygon object (a grid)
#' @rdname deprecated
#' @inheritParams receiver_density
#' @export
den_rcvs <- function(df) {
  warn_deprecated("den_rcvs", "receiver_density")
  .check_deprecated_dots(dots = list(...))

  receiver_density(gridded_receivers)
}

#' @param df data frame object consisting of results from iterative reconstruction process.
#' @rdname deprecated
#' @inheritParams depth_cutoff
#' @export
dif_co <- function(df, depth_limit) {
  warn_deprecated("dif_co", "depth_cutoff")
  .check_deprecated_dots(dots = list(...))

  depth_cutoff(reconstructed, depth_limit)
}

#' @param df data frame object consisting of results from iterative reconstruction process
#' @rdname deprecated
#' @inheritParams depth_cutoff
#' @export
gd_fts <- function(df) {
  warn_deprecated("gd_fts", "summarize_good_fits")
  .check_deprecated_dots(dots = list(...))

  summarize_good_fits(reconstructed)
}

#' @rdname deprecated
#' @export
get_or <- function(model) {
  warn_deprecated("get_or", "get_odds_ratio")

  get_odds_ratio(model)
}

#' @rdname deprecated
#' @export
gps_chr <- function(df) {
  warn_deprecated("gps_chr", "summarize_gaps")
  .check_deprecated_dots(dots = list(...))

  summarize_gaps(reconstructed)
}

#' @rdname deprecated
#' @export
gps_fld <- function(df) {
  warn_deprecated("gps_fld", "gaps_filled")
  .check_deprecated_dots(dots = list(...))

  gaps_filled(reconstructed)
}

#' @param km grid cell resolution in km. one-sided length of grid cell, assumes
#'   desired grid cell is to be squared
#' @param epsg specification for desired coordinate reference system transformation
#' @param what `"polygons"` for grid cell polygons or `"centers"` for center
#'   points of grid cells
#'
#' @rdname deprecated
#' @export
grid_res <- function(km, study_site, epsg, what) {
  warn_deprecated("grid_res", "create_grid")
  .check_deprecated_dots(dots = list(...))

  create_grid(resolution, study_site, crs, type)
}

#' @param output the resulting data frame from the iterative methods process
#' @param sig.level numeric. the desired level of significance to achieve
#' @rdname deprecated
#' @export
powr <- function(output, sig.level, power, delta, n = NULL) {
  warn_deprecated("powr", "power_analysis")
  .check_deprecated_dots(dots = list(...))

  power_analysis(results, significance_level, power, delta, n)
}

#' @rdname deprecated
#' @export
rcv_chr <- function(df) {
  warn_deprecated("rcv_chr", "receiver_summary")
  .check_deprecated_dots(dots = list(...))

  receiver_summary(reconstructed)
}
