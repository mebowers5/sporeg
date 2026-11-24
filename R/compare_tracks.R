#' Compare grid cell counts of simulated and modeled tracks
#'
#' @param tracks Tracks to which modeled tracks should be compared
#' @param stations A sf object comprised of receiver station locations with a
#'    buffer around them that represents the range of the receiver (polygons)
#' @param land_barrier A sf polygon of a barrier object around which tracks should be re-routed
#' @param hotspot_grid An sf polygon grid or list of sf polygon grids
#' @param snap_tolerance The tolerance (in meters) at which an intersection between
#'    a station and a track should snap to the station centroid. It is recommended that
#'    the snap_tolerance be equal to the station buffer size.
#' @param model Character string naming a built-in movement model (default `"crawl"`),
#'    a model specification object such as [animotum_ssm], or a custom function
#'    of the form `function(data, ...)`.
#' @param det_range Numeric quantity representing detection range of receivers
#' @param vis_graph An optional visibility graph created from the barrier object.
#'   This will be created if not provided, but parallel computations can be speed
#'   up by supplying this directly.
#' @param ... Additional arguments passed to [call_imputor]
#'
#' @seealso [pathroutr::prt_visgraph]
#'
#' @return A data frame with counts and differences by grid cell ID ("gid")
#' @export
compare_tracks <- function(
  tracks,
  stations,
  land_barrier,
  hotspot_grid,
  snap_tolerance,
  model = "crawl",
  det_range = NULL,
  vis_graph = NULL,
  ...
) {
  # Maintain detailed time info in lines to derive acoustic telemetry detection data later
  tracks <- tracks |>
    tidyr::unnest(data) |>
    sf::st_as_sf()

  modified_tracks <- tracks |>
    # Tell sf that attributes are constant in space
    sf::st_set_agr("constant") |>
    ### Find intersections of tracks and receiver stations
    # These intersections represent data that a researcher might receive from an animal tagged with an acoustic transmitter that traveled along the track that we created

    # Derive acoustic detection data through the intersection of lines and receiver ranges
    # Cannot just take the intersection because the lines self intersect
    # Take the symmetrical difference between the lines and self intersection to get the true derived detection data
    sf::st_intersection(stations$geometry) |> # x provides intersection geometry - linestrings that cross station polygons

    sf::st_set_agr("constant") |>
    sf::st_centroid() |> # Take the centroid of the linestrings
    sf::st_snap(
      sf::st_centroid(stations$geometry),
      tolerance = snap_tolerance
    ) |> # Snap the linestring centroids to the station point geometry - geometry 'x' is altered

    ### Duplicate locations can be produced by the zero movement between locations
    # at the same acoustic receiver, close together in time - solved by averaging
    # locations per time step
    dplyr::arrange(ID, uid) |>
    dplyr::summarize(
      mean_position = sf::st_combine(geometry),
      .by = c(ID, uid)
    ) |>
    sf::st_centroid()

  modified_coords <- sf::st_coordinates(modified_tracks$mean_position)

  modified_tracks$x <- modified_coords[, 1]
  modified_tracks$y <- modified_coords[, 2]

  cat("\nImputing missing locations...\n\n")
  if (is.character(model)) {
    model <- structure(list(), class = paste0("sporeg_", model[1]))
  }

  modified_tracks <- call_imputor(
    model = model,
    data = modified_tracks,
    ...
  )
  cat("\nImputation done.\n")

  # Reroute tracks
  cat("\nRerouting...")
  modified_tracks <- reroute(
    modified_tracks,
    barrier = land_barrier,
    det_range = det_range,
    vis_graph = vis_graph
  )
  cat("\nRerouting done.\n")

  # If hotspot_grid is a single data.frame, convert to list for lapply
  if (is.data.frame(hotspot_grid)) {
    hotspot_grid <- list(hotspot_grid)
  }

  # Unite geometries by ID so that you have complete tracks/lines summarized by ID for the grid cell count later
  tracks <- tracks |>
    dplyr::summarize(geometry = sf::st_union(geometry))

  cat("\nComparing tracks...")
  # Apply function over list of grid cell hotspot_grid's
  results <- lapply(hotspot_grid, FUN = function(hotspot_grid) {
    all_counts <- data.frame(
      gid = hotspot_grid$gid,
      simulated_count = sf::st_intersects(hotspot_grid, tracks) |>
        lengths(),
      modified_count = sf::st_intersects(hotspot_grid, modified_tracks) |>
        lengths()
    )

    all_counts$dif <- all_counts$simulated_count - all_counts$modified_count

    all_counts
  })
  cat("\nComparison done.\n")

  # If there was only one hotspot grid, extract it from the list.
  if (length(results) == 1) {
    results[[1]]
  } else {
    results
  }
}
