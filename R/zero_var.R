#' Remove zero variance
#'
#'This function removes grid cells that lacked any variance.
#' @param df data frame object of iterative methods results
#'
#' @return a data frame object with grid IDs that did not exhibit zero variance
#' @export
#'
#' @examples
#' # Bind the lists by grid cell resolution
#' df <- lapply(df, data.table::rbindlist, idcol = 'resolution')
#' # Bind the bound resolution by iteration
#' df <- data.table::rbindlist(df, idcol = 'iteration')
#' df <- df %>%
#' left_join(tibble(resolution = 1:4,
#' res_name = c("100km", "50km", "25km", "10km")), by = "resolution")
#' # Remove grid cells with zero variance
#' df_var <- zero_var(df)

zero_var <- function(df) {

  # Extract zero variance variables first
  zero_var_rows <- df %>%
    dplyr::group_by(res_name, gid) %>%
    dplyr::summarise(sd = sd(dif)) %>%
    dplyr::filter(sd == 0) %>%
    dplyr::select(res_name, gid) %>%
    as.list()

  # Drop zero variance rows
  df <- df %>%
    dplyr::group_by(res_name, gid) %>%
    dplyr::filter(!(gid %in% zero_var_rows$gid))

  return(df)
}
