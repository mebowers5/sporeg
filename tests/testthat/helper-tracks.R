fake_track <- function(length = 20, crs = 3857, seed = 8675309) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  data.frame(
    ID = rep("fish_1", length),
    uid = sort(sample(1:(length * 3), length, replace = F)),
    x = -8460000 + seq_len(length) * 500,
    y = 4440000 + seq_len(length) * 500
  ) |>
    sf::st_as_sf(coords = c("x", "y"), crs = crs, remove = FALSE) |>
    dplyr::rename(mean_position = geometry)
}

fake_imputation_model <- function(data, ...) {
  data |>
    dplyr::select(ID, uid, geometry = mean_position)
}
