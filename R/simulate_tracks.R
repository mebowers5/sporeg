#' Simulate tracks
#'
#' This function simulates tracks inside a specified polygon.
#' @param study_area simple feature polygon object that encompasses area where
#'   tracks are allowed to be simulated.
#' @param crs specification of desired projected coordinate reference system
#' @param anims integer. quantity of desired animals to simulate
#' @param rel_site simple feature polygon object in which simulated animals are
#'   "released" or where simulated tracks begin
#' @param initial_heading passed to initHeading in `glatos::crw_in_polygon`
#' @param theta argument from `glatos::crw_in_polygon` function
#' @param vmin numeric. minimum velocity in meters per second from which to sample step length
#' @param vmax numeric. maximum velocity in meters per second from which to sample step length
#' @param n_days integer. number of days that tracks should be simulated
#'
#' @return simple feature (multi)linestring object that represents individual simulated tracks
#' @export
#'
#' @examples
#' tracks <- simulate_tracks(study_area = fo_study_site, crs = 3857, anims = 2,
#'   rel_site = fo_rel_site, initial_heading = 0, theta = c(0, 1.74), vmin = 0.98,
#'   vmax = 1.58, n_days = 30)

simulate_tracks <- function(
  anims,
  study_area,
  theta,
  vmin,
  vmax,
  rel_site,
  crs,
  n_days,
  initial_heading
) {
  sim <- base::replicate(n = anims, expr = {
    initial_position <- sf::st_sample(
      rel_site$geometry,
      size = 1,
      type = "random",
      exact = TRUE
    ) |>
      sf::st_coordinates()

    # Sample between minimum and maximum velocity to set step length per hour for each individual
    step_length <- stats::runif(1, vmin, vmax) * 60 * 60

    glatos::crw_in_polygon(
      study_area,
      theta = theta,
      stepLen = step_length,
      initPos = c(initial_position[1], initial_position[2]),
      cartesianCRS = crs,
      nsteps = n_days * 24,
      initHeading = initial_heading
    )
  })

  names(sim) <- NULL
  simu <- sim |>
    lapply(as.data.frame) |>
    dplyr::bind_rows(.id = "ID") |>
    dplyr::group_by(ID) |>
    dplyr::mutate(uid = seq_along(ID))

  simu_coords <- sf::st_coordinates(simu$geometry)
  simu$start_x <- simu_coords[, 1]
  simu$start_y <- simu_coords[, 2]

  simu |>
    dplyr::select(-geometry) |>
    dplyr::mutate(
      end_x = dplyr::lead(start_x),
      end_y = dplyr::lead(start_y)
    ) |>
    dplyr::filter(!is.na(end_y)) |>
    tidyr::nest() |>
    dplyr::mutate(
      data = purrr::map(
        data,
        ~ dplyr::mutate(
          .x,
          geometry = purrr::pmap(
            .l = list(start_x, start_y, end_x, end_y),
            .f = make_line
          ),
          geometry = sf::st_as_sfc(geometry, crs = sf::st_crs(rel_site))
        )
      )
    )
}
