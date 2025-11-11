#' tidy_lm (deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `tidy_lm()` was renamed to `tidy_lm_results()` to better reflect that it returns
#' a tidy data frame of results from multiple linear model specifications.
#'
#' @keywords internal
#' @export
tidy_lm <- function(...) {
  lifecycle::deprecate_warn("0.1.0", "tidy_lm()", "tidy_lm_results()")
  tidy_lm_results(...)
}
