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

# @staticimports pkg:isstatic
#   has_fileext is_all_null is_unit is_patchwork is_list_of

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
#' @importFrom cliExtras cli_yesno
has_same_name_col <- function(x, col = NULL, prefix = "orig", ask = FALSE, quiet = FALSE) {
  if (rlang::has_name(x, col)) {
    new_col <- paste0(prefix, "_", col)

    if (ask && !quiet) {
      if (!cliExtras::cli_yesno(
        "The provided data includes an existing column named '{col}'.
      Do you want to proceed and rename this column to {new_col}?"
      )) {
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
