#' Remove zero variance
#'
#'This function removes grid cells that lacked any variance.
#' @param df data frame object of iterative methods results
#'
#' @return a data frame object with grid IDs that did not exhibit zero variance
#' @export
#'
#' @examples # Remove grid cells with zero variance
#' library(sporeg)
#' library(dplyr)
#' load(system.file("extdata", "results.Rda", package = "sporeg"))
#'
#' results <- lapply(results, data.table::rbindlist, idcol = 'resolution')
#' results <- data.table::rbindlist(results, idcol = 'iteration')
#' results <- results %>%
#' left_join(tibble(resolution = 1:4,
#' res_name = c("100km", "50km", "25km", "10km")),
#' by = "resolution")
#'
#' df_var <- zero_var(results)

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
