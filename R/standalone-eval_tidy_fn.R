# ---
# repo: elipousson/maplayer
# file: standalone-eval_tidy_fn.R
# last-updated: 2024-05-23
# license: https://creativecommons.org/publicdomain/zero/1.0/
# imports: [rlang]
# ---
#
# ## Changelog
#
# 2024-05-23:
# * Add missing name spacing for rlang functions and remove dependencies for external scripts.
#
# 2023-09-15:
# * Existing helper functions converted to helper standalone script.
#
# nocov start

#' Is this object a function or a formula?
#'
#' If x is a function or a formula, return `TRUE`.
#'
#' @param x A object to check.
#' @noRd
#' @importFrom rlang is_function is_formula
is_fn <- function(x) {
  rlang::is_function(x) || rlang::is_formula(x, scoped = TRUE)
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
make_fn <- function(fn,
                    ...,
                    arg = rlang::caller_arg(fn),
                    call = rlang::caller_env()) {
  rlang::check_required(fn, arg = arg, call = call)

  if (rlang::is_function(fn)) {
    return(fn)
  }

  rlang::as_function(fn, arg = arg, call = call)
}

#' Use a function or formula on an object x
#'
#' @param x Object to pass as first argument of function.
#' @inheritParams make_fn
#' @param ... Additional arguments to pass to function.
#' @noRd
use_fn <- function(x = NULL,
                   .f = NULL,
                   ...,
                   arg = rlang::caller_arg(fn),
                   call = rlang::caller_env()) {
  if (is.null(.f)) {
    return(x)
  }

  .f <- make_fn(fn = .f, arg = arg, call = call)

  .f(x, ...)
}

#' Evaluate an function with spliced parameters
#'
#' @param x Object to pass as first argument of function.
#' @param param Parameters to splice as additional arguments of function. Set to
#'   `TRUE` to execute function on x with no parameters.
#' @param pkg Package name passed to [rlang::check_installed()]
#' @inheritParams rlang::check_installed
#' @noRd
#' @importFrom rlang caller_arg caller_env is_missing eval_tidy quo is_logical
eval_tidy_fn <- function(x,
                         params = NULL,
                         pkg = NULL,
                         fn = NULL,
                         .f = NULL,
                         reason = NULL,
                         arg = rlang::caller_arg(fn),
                         env = rlang::caller_env(),
                         call = rlang::caller_env()) {
  if (rlang::is_empty(params) && !rlang::is_missing(x)) {
    return(x)
  }

  .f <- .f %||% fn

  if (!is.null(pkg)) {
    rlang::check_installed(pkg, reason = reason, call = call)
  }

  .f <- make_fn(.f, arg = arg, call = call)

  if (is.list(params)) {
    if (rlang::is_missing(x)) {
      expr <- rlang::quo(.f(!!!params))
    } else {
      expr <- rlang::quo(.f(x, !!!params))
    }

    rlang::eval_tidy(expr = expr, env = env)
  } else if (rlang::is_logical(params) && params) {
    .f(x)
  }
}

#' Modify function parameters
#'
#' @noRd
#' @importFrom rlang fn_fmls is_missing
#' @importFrom utils modifyList
modify_fn_params <- function(params,
                             fn,
                             keep_missing = FALSE,
                             keep.null = FALSE,
                             ...) {
  fmls <- rlang::fn_fmls(fn)

  if (!keep_missing) {
    missing_fmls <- vapply(fmls, rlang::is_missing, logical(1))
    fmls <- fmls[!missing_fmls]
  }

  params <- c(rlang::list2(...), params)

  utils::modifyList(
    fmls,
    params,
    keep.null = keep.null
  )
}

# nocov end
