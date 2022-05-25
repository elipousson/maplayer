
#' Group data by column if present
#'
#' @param data Data frame or simple feature object
#' @param col Column name/value
#' @noRd
group_by_col <- function(data, col = NULL) {
  if (is.null(col) || is.null(data)) {
    return(data)
  }

  if ((rlang::has_length(col, 1)) && rlang::has_name(data, col)) {
    return(dplyr::group_by(data, .data[[col]]))
  }
}

#' Add column to data if not present
#'
#' @param data Data frame or simple feature object
#' @param col Column name/value
#' @noRd
add_col <- function(data, col = NULL) {
  if (!is.null(col) && !(col %in% names(data)) && any(length(col) %in% c(nrow(data), 1))) {
    # FIXME: This is a non-standard pattern - I like it but it may or may not be appropriate and should be documented
    # TODO: Substitute dplyr::bind_cols() instead
    data[[col]] <- col
  }

  data
}

#' Modify mapping for ggplot2 aesthetics
#'
#' @param mapping aesthetic mapping to modify
#' @param data Data used to determine sf column for geometry aesthetic
#' @param ... Additional parameters with aesthetics to modify and column values
#'   to use, e.g. label = label_col
#' @noRd
modify_mapping <- function(mapping = NULL, data = NULL, ...) {
  if (is.null(mapping)) {
    mapping <-
      ggplot2::aes()
  }

  params <- rlang::list2(...)

  if (!is.null(params)) {
    if (("label" %in% names(params)) && !is.null(params$label)) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(label = .data[[params$label]]),
          mapping
        )
    }

    if (("description" %in% names(params)) && !is.null(params$description)) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(description = .data[[params$description]]),
          mapping
        )
    }

    if (("fill" %in% names(params)) && !is.null(params$fill)) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(fill = .data[[params$fill]]),
          mapping
        )
    }

    if (("color" %in% names(params)) && !is.null(params$color)) {
      mapping <-
        utils::modifyList(
          ggplot2::aes(color = .data[[params$color]]),
          mapping
        )
    }
  }

  if (!is.null(data)) {
    mapping <-
      utils::modifyList(
        ggplot2::aes(geometry = .data[[attributes(data)$sf_column]]),
        mapping
      )
  }

  return(mapping)
}

#' Apply function to data
#'
#' @param data Data to apply function to
#' @param fn defaults to NULL
#' @importFrom rlang as_function
#' @noRd
use_fn <- function(data, fn = NULL) {
  if (is.null(fn)) {
    return(data)
  }

  fn <- rlang::as_function(fn)
  fn(data)
}

#' Does the data frame has a column with the same name?
#'
#' @noRd
#' @importFrom rlang has_name
#' @importFrom cli cli_abort cli_alert_success
#' @importFrom dplyr rename
has_same_name_col <- function(x, col = NULL, prefix = "orig", ask = FALSE, quiet = FALSE) {
  if (rlang::has_name(x, col)) {
    new_col <- paste0(prefix, "_", col)

    if (ask && !quiet) {
      if (!cli_yeah("The provided data includes an existing column named '{col}'.
                   Do you want to proceed and rename this column to {new_col}?")) {
        cli::cli_abort("Please rename your column to use this function.")
      }
    }

    if (!quiet) {
      cli::cli_alert_success(
        "The existing column '{col}' to '{new_col}' to avoid overwriting any existing values."
      )
    }

    x <-
      dplyr::rename(
        x,
        "{new_col}" := col
      )
  }

  return(x)
}

#' Is this package installed?
#'
#' @param package Name of a package.
#' @param repo GitHub repository to use for the package.
#' @importFrom rlang is_installed check_installed
#' @noRd
is_pkg_installed <- function(pkg, repo = NULL) {
  if (!rlang::is_installed(pkg = pkg)) {
    if (!is.null(repo)) {
      pkg <- repo
    }

    rlang::check_installed(pkg = pkg)
  }
}

# This file contains code from the usethis R package <https://github.com/r-lib/usethis>
# The license and copyright for this package follows:
#
# MIT License
#
# Copyright (c) 2020 usethis authors
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' Modified version of [usethis::ui_yeah]
#'
#' @noRd
#' @importFrom glue glue_collapse glue
#' @importFrom rlang is_interactive
#' @importFrom cli cli_abort cli_alert
#' @importFrom utils menu
cli_yeah <- function(x,
                     yes = c("Yes", "Definitely", "For sure", "Yup", "Yeah", "I agree", "Absolutely"),
                     no = c("No way", "Not now", "Negative", "No", "Nope", "Absolutely not"),
                     n_yes = 1,
                     n_no = 2,
                     shuffle = TRUE,
                     .envir = parent.frame()) {
  x <- glue::glue_collapse(x, "\n")
  x <- glue::glue(x, .envir = .envir)

  if (!rlang::is_interactive()) {
    cli::cli_abort(
      c(
        "User input required, but session is not interactive.",
        "Query: {x}"
      )
    )
  }

  n_yes <- min(n_yes, length(yes))
  n_no <- min(n_no, length(no))
  qs <- c(sample(yes, n_yes), sample(no, n_no))

  if (shuffle) {
    qs <- sample(qs)
  }

  cli::cli_alert(x)
  out <- utils::menu(qs)
  out != 0L && qs[[out]] %in% yes
}

utils::globalVariables(
  c(
    ":=", "df", "geometry", "angle",
    "lon", "lat", "name", "repo",
    "svg_url", "x1", "x2", "y1", "y2"
  )
)
