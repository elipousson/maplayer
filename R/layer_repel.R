#' Use ggrepel to create text annotations based on simple features
#'
#' Use [ggrepel::geom_label_repel] or [ggrepel::geom_text_repel] with
#' [ggplot2::stat_sf_coordinates()] to create a layer of textual annotations
#' repelled from simple feature locations.
#'
#' @param label_col Column name to use for label aesthetic mapping. Optional if
#'   label is provided to mapping; required otherwise.
#' @inheritParams ggrepel::geom_label_repel
#' @inheritDotParams ggrepel::geom_label_repel
#' @export
#' @importFrom rlang is_character arg_match
layer_repel <- function(mapping = aes(),
                        data = NULL,
                        label_col = "name",
                        geom = c("text", "label"),
                        ...) {
  is_pkg_installed("ggrepel")

  if (!rlang::is_character(label_col, 1)) {
    cli_abort(
      "{.arg label_col} must be length 1, not {length(label_col)}."
    )
  }

  geom <- rlang::arg_match(geom)

  switch(
    geom,
      "text" = geom_sf_text_repel(mapping = mapping, data = data, ...),
      "label" = geom_sf_label_repel(mapping = mapping, data = data, ...)
    )
}

#' @name geom_sf_label_repel
#' @rdname layer_repel
geom_sf_label_repel <- function(mapping = aes(), data = NULL, ...) {
  geom_sf_coordinates(
    mapping = aes_label(mapping, data),
    data = data,
    geom = ggrepel::geom_label_repel,
    ...
    )
}

#' @name geom_sf_text_repel
#' @rdname layer_repel
geom_sf_text_repel <- function(mapping = aes(), data = NULL, ...) {
  geom_sf_coordinates(
    mapping = aes_label(mapping, data),
    data = data,
    geom = ggrepel::geom_label_repel,
    ...
  )
}

#' Helper function to add geometry to mapping and "sf_coordinates" to stat
#'
#' @noRd
geom_sf_coordinates <- function(mapping = aes(), data = NULL, geom = NULL, ...) {
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

  utils::modifyList(
    aes(geometry = .data[[sfext::get_sf_col(data) %||% sf_col]]),
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
