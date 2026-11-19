#' Reroute tracks off a barrier
#'
#' This function provides summary statistics on the densities and counts of receivers in a grid.
#' @param df A spatial data frame consisting of track points
#' @param barrier A simple feature polygon object representing a barrier
#' @param det_range Numeric quantity representing detection range of receivers
#' @param vis_graph An optional visibility graph created from the barrier object.
#'   This will be created if not provided, but parallel computations can be speed
#'   up by supplying this directly.
#' @return A data frame object with minimum, mean, and maximum of receiver densities and counts in km^-2
#' @export
#'
#' @examples
#'
#' df <- demo_dets |>
#'   dplyr::filter(
#'     Sex == "Female",
#'     met_seas == "Autumn",
#'     RC == "P") |>
#'   sf::st_transform(3857)
#'
#' reroute(df, barrier = atlcoast, det_range = 500)

reroute <- function(df, barrier, det_range = NULL, vis_graph = NULL) {
  if (is.null(vis_graph)) {
    # prt_visgraph will build our visual graph network from our land barrier object and
    # return a SpatialLinesNetwork / sfNetwork that has no edges that cross land

    cat("Creating visibility graph...\n")

    vis_graph <- barrier |>
      sf::st_set_agr("constant") |> # silence warning about other variables being constant over space
      pathroutr::prt_visgraph()

    cat("DONE\n")
  }

  rerouted <- df |>
    dplyr::group_split() |>
    lapply(function(.) {
      # the track cannot start or end within the land barrier; prt_trim() trims those out
      trimmed_points <- pathroutr::prt_trim(., barrier)

      # here, we create our re-routed points; the return is a two column data frame with the
      # index location in the original point data and the new geometry. The user can handle
      # updating of those original point data or pass the result on to prt_update_points()
      trimmed_points |>
        pathroutr::prt_reroute(barrier, vis_graph) |>
        pathroutr::prt_update_points(trimmed_points) |>
        sf::st_coordinates() |>
        sf::st_linestring()
    })

  rerouted <- sf::st_sf(
    as.data.frame(dplyr::group_keys(df)),
    geometry = sf::st_sfc(rerouted, crs = sf::st_crs(df))
  )

  if (!is.null(det_range)) {
    rerouted <- rerouted |>
      sf::st_buffer(det_range, endCapStyle = "ROUND", joinStyle = "ROUND")
  }
  rerouted
}
