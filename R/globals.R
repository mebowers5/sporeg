utils::globalVariables(
  c(
    "data",
    "x",
    "ID",
    "time",
    "mean_pos",
    "locType",
    "mu.x",
    "mu.y",
    "speed",
    "trim_data",
    "rrt_pts",
    "trim_data",
    "path_pts",
    "geom",
    "geometry",
    "dif",
    "d_shore",
    "count",
    "gid2",
    "den_rcs",
    "km",
    "gid",
    "altitude",
    "mean_depth",
    "n",
    "g_fit",
    "d_shore_km",
    "p_a",
    "n_abs",
    "Effect",
    "AnimalID",
    "uid",
    "POINT_Y",
    "POINT_X",
    "end_y",
    "res_name",
    "sd",
    "den_rcs",
    "depth_limit",
    "tot"
  )
)

#' Need to have doFuture in Imports due to momentuHMM, but we never actually call it.
#'  This causes a NOTE in R CMD check
silence_doFuture_note <- function() doFuture::registerDoFuture()
