#' Does object inherit class "gg" or is it a last that inherits a gg class?
#'
#' @noRd
obj_is_gg <- function(x) {
  is_gg(x) || is_gg_list(x)
}


#' Does object seem to be a neatline layer?
#'
#' @noRd
is_neatline <- function(x) {
  obj_is_gg(x) && (ggplot2::is.Coord(x) || any(vapply(x, ggplot2::is.Coord, FALSE)))
}


#' Helper function to add geometry to mapping and "sf_coordinates" to stat
#'
#' @noRd
geom_sf_coordinates <- function(mapping = aes(),
                                data = NULL,
                                geom = NULL,
                                .envir = parent.frame(),
                                call = .envir,
                                ...) {
  mapping <- mapping %||% aes()
  geom(
    mapping = aes_sf_coords(mapping, data),
    data = data,
    stat = "sf_coordinates",
    ...
  )
}


#' Helper function to add geometry to mapping
#'
#' @noRd
#' @importFrom rlang has_name
#' @importFrom sfext get_sf_col
#' @importFrom utils modifyList
aes_sf_coords <- function(mapping = aes(), data = NULL, sf_col = "geometry") {
  if (rlang::has_name(mapping, "geometry")) {
    return(mapping)
  }

  sf_col <- sfext::get_sf_col(data) %||% sf_col

  utils::modifyList(
    aes(geometry = .data[[sf_col]]),
    mapping
  )
}


#' Helper function to add label to mapping
#'
#' @noRd
#' @importFrom rlang has_name
#' @importFrom utils modifyList
aes_label <- function(mapping = aes(), data = NULL, label_col = "name") {
  if (rlang::has_name(mapping, "label")) {
    return(mapping)
  }

  utils::modifyList(
    ggplot2::aes(label = .data[[label_col]]),
    mapping
  )
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

  params <- list2(...)

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

  if (is.null(data)) {
    return(mapping)
  }

  utils::modifyList(
    ggplot2::aes(geometry = .data[[attributes(data)$sf_column]]),
    mapping
  )
}


#' Use smoothr::smooth on data
#'
#' See utils-fn.R for info on eval_tidy_fn
#'
#' @noRd
with_smooth <- function(x, params = NULL) {
  eval_tidy_fn(x, params, "smoothr", smoothr::smooth)
}


#' Use smoothr::smooth on ggplot2
#'
#' See utils-fn.R for info on eval_tidy_fn
#'
#' @noRd
with_shadow <- function(x, params = NULL) {
  eval_tidy_fn(x, params, "ggfx", ggfx::with_shadow)
}
