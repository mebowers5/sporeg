utils::globalVariables(
  c(
    "count",
    "data",
    "den_rcs",
    "depth_limit",
    "dif",
    "d_shore",
    "d_shore_km",
    "Effect",
    "end_y",
    "g_fit",
    "geom",
    "geometry",
    "gid",
    "gid2",
    "ID",
    "locType",
    "mean_depth",
    "mean_position",
    "mu.x",
    "mu.y",
    "n",
    "n_abs",
    "p_a",
    "path_pts",
    "rrt_pts",
    "sd",
    "speed",
    "start_x",
    "start_y",
    "study_yr",
    "time",
    "tot",
    "trim_data",
    "uid",
    "x"
  )
)

#' Need to have doFuture in Imports due to momentuHMM, but we never actually call it.
#'  This causes a NOTE in R CMD check
silence_doFuture_note <- function() doFuture::registerDoFuture()
