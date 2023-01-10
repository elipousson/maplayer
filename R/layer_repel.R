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
#' @param location_lims A `sf`, `sfc`, or `bbox` object to use in setting xlim
#'   and ylim values if no xlim and ylim value area provided. Using this
#'   parameter constrains labels to the bounding box of `location_lims`.
#' @inheritParams ggrepel::geom_label_repel
#' @inheritDotParams ggrepel::geom_label_repel -stat
#' @export
#' @importFrom cliExtras cli_abort_ifnot
#' @importFrom rlang is_character arg_match
#' @importFrom sfext as_bbox sf_bbox_point
layer_repel <- function(mapping = aes(),
                        data = NULL,
                        label_col = "name",
                        geom = c("text", "label"),
                        location_lims = NULL,
                        xlim = c(NA, NA),
                        ylim = c(NA, NA),
                        ...) {
  cliExtras::cli_abort_ifnot(
      "{.arg label_col} must be length 1, not {length(label_col)}.",
      condition = rlang::is_character(label_col, 1)
    )

  if (!is.null(location_lims) && all(is.na(c(ylim, xlim)))) {
    bbox <- sfext::as_bbox(location_lims)

    xlim <- sfext::sf_bbox_point(bbox, c("xmin", "xmax"))[[1]]
    ylim <- sfext::sf_bbox_point(bbox, c("ymin", "ymax"))[[1]]

    xlim <- c(xlim[[1]], xlim[[2]])
    ylim <- c(ylim[[1]], ylim[[2]])
  }

  geom <- rlang::arg_match(geom)

  switch(geom,
    "text" = geom_sf_text_repel(
      mapping = mapping,
      data = data,
      label_col = label_col,
      xlim = xlim,
      ylim = ylim,
      ...
    ),
    "label" = geom_sf_label_repel(
      mapping = mapping,
      data = data,
      label_col = label_col,
      xlim = xlim,
      ylim = ylim,
      ...
    )
  )
}

#' @name geom_sf_label_repel
#' @rdname layer_repel
geom_sf_label_repel <- function(mapping = aes(), data = NULL, label_col = "name", ...) {
  is_pkg_installed("ggrepel")

  geom_sf_coordinates(
    mapping = aes_label(mapping, data, label_col),
    data = data,
    geom = ggrepel::geom_label_repel,
    ...
  )
}

#' @name geom_sf_text_repel
#' @rdname layer_repel
geom_sf_text_repel <- function(mapping = aes(), data = NULL, label_col = "name", ...) {
  is_pkg_installed("ggrepel")

  geom_sf_coordinates(
    mapping = aes_label(mapping, data, label_col),
    data = data,
    geom = ggrepel::geom_label_repel,
    ...
  )
}
