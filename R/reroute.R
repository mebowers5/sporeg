#' Reroute tracks off a barrier
#'
#' This function provides summary statistics on the densities and counts of receivers in a grid.
#' @param df A data frame output from a movement model including FID, time, mu.x, mu.y, and study_yr
#' @param barrier A simple feature polygon object representing a barrier
#' @param det_range Numeric quantity representing detection range of receivers
#' @param epsg epsg code for desired coordinated system transformation
#'
#' @return A data frame object with minimum, mean, and maximum of receiver densities and counts in km^-2
#' @export
#'
#' @examples
#'
#' df <- sporeg::demo_dets |>
#' dplyr::filter(ID %in% c("3", "4", "5") & Sex == "Female" & met_seas == "Autumn" & RC == "P") |>
#' sf::st_transform(4269)
#'
#' reroute(df, barrier = atlcoast, det_range = 500, epsg = 3857)

reroute <- function(df, barrier, det_range, epsg) {
  # Testing code, which should be removed.
  # t$path_lines <- t$geom <- NULL
  # epsg <- 3857

  # prt_visgraph will build our visual graph network from our land barrier object and
  # return a SpatialLinesNetwork / sfNetwork that has no edges that cross land
  vis_graph <- barrier |>
    sf::st_transform(epsg) |>
    pathroutr::prt_visgraph()

  track_path <- df |>
    dplyr::mutate(time = lubridate::ymd_hms(time), ID = as.factor(ID)) |>
    dplyr::select(ID, time, mu.x, mu.y, study_yr) |>
    sf::st_as_sf(coords = c("mu.x", "mu.y"), epsg) |>
    dplyr::arrange(ID, time)

  # there are multiple paths identified by ID; we'll group and nest for a proper
  # tidyverse/list-column workflow
  track_path <- track_path |>
    dplyr::group_by(ID, study_yr) |> # Must group by study year to avoid connecting last point from previous season to first point of current season
    tidyr::nest()

  # the track cannot start or end within the land barrier; prt_trim() trims those out
  track_path <- track_path |>
    dplyr::rowwise() |>
    dplyr::mutate(trim_data = list(pathroutr::prt_trim(data, barrier)))

  # here, we create our re-routed points; the return is a two column data frame with the
  # index location in the original point data and the new geometry. The user can handle
  # updating of those original point data or pass the result on to prt_update_points()
  t <- track_path |>
    dplyr::rowwise() |>
    dplyr::mutate(
      rrt_pts = list(pathroutr::prt_reroute(trim_data, barrier, vis_graph))
    )

  # NOTE: previous versions of prt_update_points() had the argument order reversed from
  # what it now requires. The updated geometry points are passed first (here, `rrt_pts`)
  # and, then, the original data to be updated. This order should allow for easy piping
  # from prt_reroute()
  t <- t |>
    dplyr::rowwise() |>
    dplyr::mutate(
      path_pts = list(pathroutr::prt_update_points(rrt_pts, trim_data)),
      path_lines = list(
        path_pts |>
          dplyr::summarise(do_union = FALSE) |>
          sf::st_cast('LINESTRING')
      )
    ) # do_union MUST be FALSE!

  # we need to rbind all of our lines and points into single objects that can be plotted
  # Need to maintain "ID" column here - THIS WORKS!
  tee$geom <- do.call(rbind, tee$path_lines)
  tee$geom <- sf::st_set_crs(tee$geom, epsg)

  tee <- tee |> dplyr::select(ID, geom)

  ### Create grid that maintains row for each grid cell
  #Convert from rowwise_df to sf object
  tee <- tee |>
    dplyr::rowwise() |>
    dplyr::mutate(geom = sf::st_geometry(geom)) |>
    dplyr::ungroup() |>
    sf::st_as_sf(sf_column_name = "geom")

  teebuff <- tee |>
    sf::st_buffer(det_range, endCapStyle = "ROUND", joinStyle = "ROUND")

  return(teebuff)
}
