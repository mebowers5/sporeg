#' Remove zero variance
#'
#'This function removes grid cells that lacked any variance.
#' @param reconstructed data frame object of iterative methods results
#'
#' @return a data frame object with grid IDs that did not exhibit zero variance
#' @export
#'
#' @examples
#' # Remove grid cells with zero variance
#' res <- lapply(sporeg::results, `[[`, 1) |>
#'   dplyr::bind_rows() |>
#'   dplyr::mutate(res_name = "100km") |>
#'   # Add 50 rows with no variance for demonstration purposes
#'   dplyr::bind_rows(
#'     data.frame(gid = rep(100, 50), dif = rep(100, 50), res_name = "100km")
#'   )
#'
#' df_var <- drop_zero_variance(res)
#' nrow(res) > nrow(df_var)

drop_zero_variance <- function(reconstructed) {
  # Extract zero variance variables first
  zero_var_rows <- reconstructed |>
    dplyr::group_by(res_name, gid) |>
    dplyr::summarise(sd = sd(dif)) |>
    dplyr::filter(sd == 0) |>
    dplyr::select(res_name, gid) |>
    as.list()

  # Drop zero variance rows
  reconstructed <- reconstructed |>
    dplyr::group_by(res_name, gid) |>
    dplyr::filter(!(gid %in% zero_var_rows$gid))

  return(reconstructed)
}
