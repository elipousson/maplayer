.onLoad <- function(lib, pkg) {
  utils::data(
    list = c("map_icons"),
    package = pkg,
    envir = parent.env(environment())
  )
}

utils::globalVariables(
  c(
    "df", "geometry", "angle", "lon", "lat", "name", "repo",
    "x1", "x2", "y1", "y2", "x", "y"
  )
)

#' Group data by column if present
#'
#' @param data Data frame or simple feature object
#' @param col Column name/value
#' @importFrom rlang has_length has_name
#' @importFrom dplyr group_by
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


#' Does the data frame has a column with the same name?
#'
#' @noRd
#' @importFrom dplyr rename
#' @importFrom rlang has_name
has_same_name_col <- function(x, col = NULL, prefix = "orig", ask = FALSE, quiet = FALSE) {
  if (rlang::has_name(x, col)) {
    new_col <- paste0(prefix, "_", col)

    if (ask && !quiet) {
      if (!cli_yeah(
      "The provided data includes an existing column named '{col}'.
      Do you want to proceed and rename this column to {new_col}?")) {
        cli_abort("Please rename your column to use this function.")
      }
    }

    if (!quiet) {
      cli_inform(
        c("v" = "The existing column '{col}' to '{new_col}' to avoid
          overwriting any existing values.")
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
#' @param package Package name.
#' @param repo Package repository passed to pkg parameter of
#'   [rlang::check_installed].
#' @noRd
#' @importFrom rlang is_installed check_installed
is_pkg_installed <- function(pkg = NULL, repo = NULL) {
  if (!is.null(pkg) && !rlang::is_installed(pkg = pkg)) {
    rlang::check_installed(pkg = repo %||% pkg)
  }
}

#' Is the package needed for these selected geom functions installed?
#'
#' @param geom geom function name as character.
#' @noRd
is_geom_pkg_installed <- function(geom) {
  ggpattern_geoms <- c("pattern", "sf_pattern")
  ggrepel_geoms <- c("text_repel", "label_repel")
  geomtextpath_geoms <- c("textsf", "labelsf")

  # Check if packages are available for text/label geoms
  if (geom %in% geomtextpath_geoms) {
    return(is_pkg_installed("geomtextpath"))
  }

  if (geom %in% ggrepel_geoms) {
    return(is_pkg_installed("ggrepel"))
  }

  if (geom %in% ggpattern_geoms) {
    return(is_pkg_installed("ggpattern"))
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
#' @importFrom rlang is_interactive
#' @importFrom utils menu
cli_yeah <- function(x,
                     yes = c("Yes", "Definitely", "For sure", "Yup", "Yeah", "I agree", "Absolutely"),
                     no = c("No way", "Not now", "Negative", "No", "Nope", "Absolutely not"),
                     n_yes = 1,
                     n_no = 2,
                     shuffle = TRUE,
                     .envir = parent.frame()) {
  x <- glue_collapse(x, "\n")
  x <- glue(x, .envir = .envir)

  if (!rlang::is_interactive()) {
    cli_abort(
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

  cli_inform(x)
  out <- utils::menu(qs)
  out != 0L && qs[[out]] %in% yes
}
