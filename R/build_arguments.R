#' Build function arguments for internal modeling methods
#'
#' Combines argument lists from multiple sources for forwarding to downstream
#' modeling or simulation functions (e.g., via [base::do.call()]). Arguments are
#' merged in ascending order of precedence: `default` < `model` < `dots` <
#' `mandatory`. Any `NULL` input layer is safely treated as an empty list, and
#' explicit `NULL` parameter values within layers are preserved.
#'
#' @param model A model or simulator specification object (typically a classed
#'   list created by a constructor such as [animotum_ssm()] or [crawl()]), a
#'   plain named list, or `NULL`.
#' @param mandatory A named list of required arguments that cannot be overridden
#'   by `model`, `default`, or `dots`. Defaults to `list()`.
#' @param default A named list of fallback default arguments used when not
#'   specified in `model`, `dots`, or `mandatory`. Defaults to `list()`.
#' @param dots A named list of user-supplied overrides (typically captured via
#'   `list(...)`). Defaults to `list()`.
#'
#' @return A single named `list` of merged arguments ready to be passed to
#'   [base::do.call()].
#'
#' @keywords internal
#' @noRd
build_arguments <- function(
  model,
  mandatory = list(),
  default = list(),
  dots = list()
) {
  to_list <- function(x) if (is.null(x)) list() else as.list(x)

  arguments <- utils::modifyList(
    to_list(default),
    to_list(unclass(model)),
    keep.null = TRUE
  )
  arguments <- utils::modifyList(arguments, to_list(dots), keep.null = TRUE)
  arguments <- utils::modifyList(
    arguments,
    to_list(mandatory),
    keep.null = TRUE
  )

  arguments
}
