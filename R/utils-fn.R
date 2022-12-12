#' Is this object a function or a formula?
#'
#' If x is a function or a formula, return `TRUE`.
#'
#' @param x A object to check.
#' @noRd
#' @importFrom rlang is_function is_formula
is_fn <- function(x) {
  rlang::is_function(x) | rlang::is_formula(x)
}

#' Make a function from a formula
#'
#' @param fn Either a formula or function.
#'
#' If a __function__, it is used as is.
#'
#' If a __formula__, e.g. `~ .x + 2`, it is converted to a function.
#'
#' @inheritParams cli::cli_abort
#' @noRd
#' @importFrom rlang is_function is_formula as_function caller_env caller_arg
make_fn <- function(fn, ..., arg = caller_arg(fn), call = caller_env()) {
  if (rlang::is_function(fn)) {
    return(fn)
  }

  if (!rlang::is_formula(fn)) {
    cli_abort(
      c("{.arg {arg}} must be a formula or function.",
        "i" = "You've supplied a {class(fn)} object."
      ),
      arg = arg,
      call = call
    )
  }

  rlang::as_function(fn)
}

#' Use a function or formula on an object x
#'
#' @param x Object to pass as first argument of function.
#' @inheritParams make_fn
#' @param ... Additional arguments to pass to function.
#' @noRd
use_fn <- function(x = NULL, fn = NULL, ..., arg = caller_arg(fn), call = caller_env()) {
  if (is.null(fn)) {
    return(x)
  }

  fn <- make_fn(fn, arg = arg, call = call)

  fn(x, ...)
}

#' Evaluate an function with spliced parameters
#'
#' @param x Object to pass as first argument of function.
#' @param param Parameters to splice as additional arguments of function.
#' @param pkg Package name passed to [is_pkg_installed]
#' @noRd
#' @importFrom rlang caller_arg caller_env is_missing eval_tidy quo is_logical
eval_tidy_fn <- function(x, params = NULL, pkg = NULL, fn = NULL, arg = caller_arg(fn), call = caller_env()) {
  if (is.null(params) && !rlang::is_missing(x)) {
    return(x)
  }

  is_pkg_installed(pkg)

  fn <- make_fn(fn, arg = arg, call = call)

  if (is.list(params)) {
    if (rlang::is_missing(x)) {
      fn_quo <- rlang::quo(fn(!!!params))
    } else {
      fn_quo <- rlang::quo(fn(x, !!!params))
    }

    rlang::eval_tidy(fn_quo)
  } else if (rlang::is_logical(params) && params) {
    fn(x)
  }
}
