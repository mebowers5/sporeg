#' Simulate tracks
#'
#' This function simulates tracks inside a specified polygon.
#' @param study_area simple feature polygon object that encompasses area where
#'   tracks are allowed to be simulated.
#' @param anims integer. quantity of desired animals to simulate
#' @param start_area simple feature polygon object in which simulated animals are
#'   "released" or where simulated tracks begin
#' @param n_days integer. number of days that tracks should be simulated
#'
#' @return simple feature (multi)linestring object that represents individual simulated tracks
#' @export
#'
#' @examples
#' tracks <- simulate_tracks(study_area = fo_study_site, anims = 2,
#'   start_area = fo_rel_site, initial_heading = 0, theta = c(0, 1.74), vmin = 0.98,
#'   vmax = 1.58, n_days = 30)

simulate_tracks <- function(
  anims,
  start_area,
  study_area,
  n_days,
  model = "crw_in_polygon",
  ...
) {
  initial_positions <- sf::st_sample(
    start_area$geometry,
    size = anims,
    type = "random",
    exact = TRUE
  ) |>
    sf::st_coordinates()

  if (is.character(model)) {
    model <- structure(list(), class = paste0("sporeg_", model[1]))
  }

  sim <- call_simulator(
    model = model,
    anims = anims,
    study_area = study_area,
    n_days = n_days,
    initial_positions = initial_positions,
    ...
  )

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
          geometry = sf::st_as_sfc(geometry, crs = sf::st_crs(start_area))
        )
      )
    )
}
