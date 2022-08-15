#' Use ggrepel to create text annotations based on simple features
#'
#' Use [ggrepel::geom_label_repel] or [ggrepel::geom_text_repel] with
#' [ggplot2::stat_sf_coordinates()] to create a layer of textual annotations
#' repelled from simple feature locations.
#'
#' @param label_col Column name to use for label aesthetic mapping. Optional if
#'   label is provided to mapping; required otherwise.
#' @param geom Character vector with geom to use, "text" for
#'   [ggrepel::geom_text_repel()] or "label" for [ggrepel::geom_label_repel()].
#' @inheritParams ggrepel::geom_label_repel
#' @inheritDotParams ggrepel::geom_label_repel -stat
#' @export
#' @importFrom rlang is_character arg_match
layer_repel <- function(mapping = aes(),
                        data = NULL,
                        label_col = "name",
                        geom = c("text", "label"),
                        ...) {
  if (!rlang::is_character(label_col, 1)) {
    cli_abort(
      "{.arg label_col} must be length 1, not {length(label_col)}."
    )
  }

  geom <- rlang::arg_match(geom)

  switch(geom,
    "text" = geom_sf_text_repel(mapping = mapping, data = data, ...),
    "label" = geom_sf_label_repel(mapping = mapping, data = data, ...)
  )
}

#' @name geom_sf_label_repel
#' @rdname layer_repel
geom_sf_label_repel <- function(mapping = aes(), data = NULL, ...) {
  is_pkg_installed("ggrepel")

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
  is_pkg_installed("ggrepel")

  geom_sf_coordinates(
    mapping = aes_label(mapping, data),
    data = data,
    geom = ggrepel::geom_label_repel,
    ...
  )
}
