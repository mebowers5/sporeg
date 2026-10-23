#' Reroute tracks off a barrier
#'
#' This function provides summary statistics on the densities and counts of receivers in a grid.
#' @param df A data frame output from a movement model including ID, time, mu.x, mu.y, and study_yr
#' @param sex A character representing sex of the animal as in "Female"
#' @param season A character representing meteorological season as in "Autumn"
#' @param rc A character representing reproductive cycle as in "P" for pregnant or "NP" for not pregnant
#' @param barrier A simple feature polygon object representing a barrier
#' @param det_range Numeric quantity representing detection range of receivers
#'
#' @return A data frame object with minimum, mean, and maximum of receiver densities and counts in km^-2
#' @export
#'
#' @examples
#'
#' df <- sporeg::demo_dets |> dplyr::filter(FID %in% c("4", "5", "6")) |> sf::st_transform(4269)
#'
#' reroute(df, sex = "Female", season = "Autumn", rc = "P", barrier = sporeg::atlcoast, det_range = 500)

reroute <- function(df, sex, season, rc, barrier, det_range) {
  # prt_visgraph will build our visual graph network from our land barrier object and
  # return a SpatialLinesNetwork / sfNetwork that has no edges that cross land
  vis_graph <- pathroutr::prt_visgraph(barrier)

  t$path_lines <- t$geom <- NULL

  if(missing(rc) && missing(season)) {
    track_path <- df |>
      dplyr::mutate(time = lubridate::ymd_hms(time, truncated = 3),
                    ID = as.factor(ID)) |>
      dplyr::select(ID, time, mu.x, mu.y, study_yr) |>
      sf::st_as_sf(coords = c("mu.x","mu.y"), crs = 3857) |>
      arrange(ID, time)

    # there are multiple paths identified by ID; we'll group and nest for a proper
    # tidyverse/list-column workflow
    track_path <- track_path |>
      group_by(ID, study_yr) |> # Must group by study year to avoid connecting last point from previous season to first point of current season
      tidyr::nest()

    # the track cannot start or end within the land barrier; prt_trim() trims those out
    track_path <- track_path |>
      rowwise() |>
      mutate(trim_data = list(pathroutr::prt_trim(data, barrier)))

    # here, we create our re-routed points; the return is a two column data frame with the
    # index location in the original point data and the new geometry. The user can handle
    # updating of those original point data or pass the result on to prt_update_points()
    t <- track_path |> dplyr::rowwise() |>
      mutate(rrt_pts = list(prt_reroute(trim_data, barrier, vis_graph)))

    # NOTE: previous versions of prt_update_points() had the argument order reversed from
    # what it now requires. The updated geometry points are passed first (here, `rrt_pts`)
    # and, then, the original data to be updated. This order should allow for easy piping
    # from prt_reroute()
    t <- t |> dplyr::rowwise() |>
      mutate(path_pts = list(prt_update_points(rrt_pts, trim_data)),
             path_lines = list(path_pts |> summarise(do_union = FALSE) |> sf::st_cast('LINESTRING')))  # do_union MUST be FALSE!

    # we need to rbind all of our lines and points into single objects that can be plotted
    # Need to maintain "ID" column here - THIS WORKS!
    t$geom <- do.call(rbind, t$path_lines)
    t$geom <- sf::st_set_crs(t$geom, 3857)

    t <- t |> dplyr::select(ID, geom)

    ### Create grid that maintains row for each grid cell
    #Convert from rowwise_df to sf object
    t <- t |>
      rowwise() |>
      mutate(geom = sf::st_geometry(geom)) |>
      ungroup() |>
      st_as_sf(., sf_column_name = "geom")

    tbuff <- t |>
      sf::st_buffer(., det_range, endCapStyle = "ROUND", joinStyle = "ROUND")

    return(tbuff)

  }

  if(missing(rc)) {
    track_path <- df |>
      dplyr::mutate(time = lubridate::ymd_hms(time, truncated = 3),
                    ID = as.factor(ID)) |>
      dplyr::filter(Sex == sex & met_seas == season) |>
      dplyr::select(ID, time, mu.x, mu.y, study_yr) |>
      sf::st_as_sf(coords = c("mu.x","mu.y"), crs = 3857) |>
      arrange(ID, time)

    # there are multiple paths identified by ID; we'll group and nest for a proper
    # tidyverse/list-column workflow
    track_path <- track_path |>
      group_by(ID, study_yr) |> # Must group by study year to avoid connecting last point from previous season to first point of current season
      tidyr::nest()

    # the track cannot start or end within the land barrier; prt_trim() trims those out
    track_path <- track_path |>
      rowwise() |>
      mutate(trim_data = list(pathroutr::prt_trim(data, barrier)))

    # here, we create our re-routed points; the return is a two column data frame with the
    # index location in the original point data and the new geometry. The user can handle
    # updating of those original point data or pass the result on to prt_update_points()
    t <- track_path |> dplyr::rowwise() |>
      mutate(rrt_pts = list(prt_reroute(trim_data, barrier, vis_graph)))

    # NOTE: previous versions of prt_update_points() had the argument order reversed from
    # what it now requires. The updated geometry points are passed first (here, `rrt_pts`)
    # and, then, the original data to be updated. This order should allow for easy piping
    # from prt_reroute()
    t <- t |> dplyr::rowwise() |>
      mutate(path_pts = list(prt_update_points(rrt_pts, trim_data)),
             path_lines = list(path_pts |> summarise(do_union = FALSE) |> sf::st_cast('LINESTRING')))  # do_union MUST be FALSE!

    # we need to rbind all of our lines and points into single objects that can be plotted
    # Need to maintain "ID" column here - THIS WORKS!
    t$geom <- do.call(rbind, t$path_lines)
    t$geom <- sf::st_set_crs(t$geom, 3857)

    t <- t |> dplyr::select(ID, geom)

    ### Create grid that maintains row for each grid cell
    #Convert from rowwise_df to sf object
    t <- t |>
      rowwise() |>
      mutate(geom = sf::st_geometry(geom)) |>
      ungroup() |>
      st_as_sf(., sf_column_name = "geom")

    tbuff <- t |>
      sf::st_buffer(., det_range, endCapStyle = "ROUND", joinStyle = "ROUND")

    return(tbuff)

  }
  if(missing(season)) {
    track_path <- df |>
      dplyr::mutate(time = lubridate::ymd_hms(time), ID = as.factor(ID)) |>
      dplyr::filter(Sex == sex & RC == rc) |>
      dplyr::select(ID, time, mu.x, mu.y, study_yr) |>
      sf::st_as_sf(coords = c("mu.x","mu.y"), crs = 3857) |>
      arrange(ID, time)

    # there are multiple paths identified by ID; we'll group and nest for a proper
    # tidyverse/list-column workflow
    track_path <- track_path |>
      group_by(ID, study_yr) |> # Must group by study year to avoid connecting last point from previous season to first point of current season
      tidyr::nest()

    # the track cannot start or end within the land barrier; prt_trim() trims those out
    track_path <- track_path |>
      rowwise() |>
      mutate(trim_data = list(pathroutr::prt_trim(data, barrier)))

    # here, we create our re-routed points; the return is a two column data frame with the
    # index location in the original point data and the new geometry. The user can handle
    # updating of those original point data or pass the result on to prt_update_points()
    t <- track_path |> dplyr::rowwise() |>
      mutate(rrt_pts = list(prt_reroute(trim_data, barrier, vis_graph)))

    # NOTE: previous versions of prt_update_points() had the argument order reversed from
    # what it now requires. The updated geometry points are passed first (here, `rrt_pts`)
    # and, then, the original data to be updated. This order should allow for easy piping
    # from prt_reroute()
    t <- t |> dplyr::rowwise() |>
      mutate(path_pts = list(prt_update_points(rrt_pts, trim_data)),
             path_lines = list(path_pts |> summarise(do_union = FALSE) |> sf::st_cast('LINESTRING')))  # do_union MUST be FALSE!

    # we need to rbind all of our lines and points into single objects that can be plotted
    # Need to maintain "ID" column here - THIS WORKS!
    t$geom <- do.call(rbind, t$path_lines)
    t$geom <- sf::st_set_crs(t$geom, 3857)

    t <- t |> dplyr::select(ID, geom)

    ### Create grid that maintains row for each grid cell
    #Convert from rowwise_df to sf object
    t <- t |>
      rowwise() |>
      mutate(geom = sf::st_geometry(geom)) |>
      ungroup() |>
      st_as_sf(., sf_column_name = "geom")

    tbuff <- t |>
      sf::st_buffer(., det_range, endCapStyle = "ROUND", joinStyle = "ROUND")

    return(tbuff)

  } else {

    track_path <- df |>
      dplyr::mutate(time = lubridate::ymd_hms(time), ID = as.factor(ID)) |>
      dplyr::filter(Sex == sex & met_seas == season & RC == rc) |>
      dplyr::select(ID, time, mu.x, mu.y, study_yr) |>
      sf::st_as_sf(coords = c("mu.x","mu.y"), crs = 3857) |>
      arrange(ID, time)

    # there are multiple paths identified by ID; we'll group and nest for a proper
    # tidyverse/list-column workflow
    track_path <- track_path |>
      group_by(ID, study_yr) |> # Must group by study year to avoid connecting last point from previous season to first point of current season
      tidyr::nest()

    # the track cannot start or end within the land barrier; prt_trim() trims those out
    track_path <- track_path |>
      rowwise() |>
      mutate(trim_data = list(pathroutr::prt_trim(data, barrier)))

    # here, we create our re-routed points; the return is a two column data frame with the
    # index location in the original point data and the new geometry. The user can handle
    # updating of those original point data or pass the result on to prt_update_points()
    t <- track_path |> dplyr::rowwise() |>
      mutate(rrt_pts = list(prt_reroute(trim_data, barrier, vis_graph)))

    # NOTE: previous versions of prt_update_points() had the argument order reversed from
    # what it now requires. The updated geometry points are passed first (here, `rrt_pts`)
    # and, then, the original data to be updated. This order should allow for easy piping
    # from prt_reroute()
    t <- t |> dplyr::rowwise() |>
      mutate(path_pts = list(prt_update_points(rrt_pts, trim_data)),
             path_lines = list(path_pts |> summarise(do_union = FALSE) |> sf::st_cast('LINESTRING')))  # do_union MUST be FALSE!

    # we need to rbind all of our lines and points into single objects that can be plotted
    # Need to maintain "ID" column here - THIS WORKS!
    t$geom <- do.call(rbind, t$path_lines)
    t$geom <- sf::st_set_crs(t$geom, 3857)

    t <- t |> dplyr::select(ID, geom)

    ### Create grid that maintains row for each grid cell
    #Convert from rowwise_df to sf object
    t <- t |>
      rowwise() |>
      mutate(geom = sf::st_geometry(geom)) |>
      ungroup() |>
      st_as_sf(., sf_column_name = "geom")

    tbuff <- t |>
      sf::st_buffer(., det_range, endCapStyle = "ROUND", joinStyle = "ROUND")

    return(tbuff)
  }
}
