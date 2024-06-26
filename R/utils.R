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
#   has_fileext is_all_null is_unit is_patchwork is_gg is_list_all is_gg_list
#   str_extract_fileext str_remove_fileext

#' @keywords internal
#' @importFrom rlang zap current_env
#' @importFrom vctrs vec_cbind
list_cbind <- function(x,
                       name_repair = c("unique", "universal", "check_unique"),
                       size = NULL) {
  vctrs::vec_cbind(
    !!!x,
    .name_repair = name_repair,
    .size = size,
    .error_call = rlang::current_env()
  )
}

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
  if (is.null(col)) {
    return(data)
  }

  if (!(col %in% names(data)) && any(length(col) %in% c(nrow(data), 1))) {
    # FIXME: This is a non-standard pattern - I like it but it may or may not be
    # appropriate and should be documented
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
check_dev_installed <- function(pkg = NULL, repo = NULL) {
  if (!is.null(pkg) && !rlang::is_installed(pkg = pkg)) {
    rlang::check_installed(pkg = repo %||% pkg)
  }
}

#' Is the package needed for these selected geom functions installed?
#'
#' @param geom geom function name as character.
#' @noRd
check_geom_installed <- function(geom) {
  # Check if packages are available for text/label geoms
  if (geom %in% c("text_repel", "label_repel")) {
    return(rlang::check_installed("ggrepel"))
  }

  if (geom %in% c("textsf", "labelsf")) {
    return(rlang::check_installed("geomtextpath"))
  }

  if (geom %in% c("pattern", "sf_pattern")) {
    return(rlang::check_installed("ggpattern"))
  }

  if (geom == "arrowsegment") {
    return(check_dev_installed("ggarchery", "mdhall272/ggarchery"))
  }

  if (geom %in% c("diagonal0", "geom_link")) {
    return(rlang::check_installed("ggforce"))
  }
}
