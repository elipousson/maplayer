#' Set map limits to a bounding box with a buffer and set aspect ratio
#'
#' Set limits for a map to the bounding box of a feature using
#' [ggplot2::coord_sf()]. Optionally, adjust the x size by applying a buffer
#' and/or adjust the aspect ratio of the limiting bounding box to match a set
#' aspect ratio.
#'
#' @param data A `sf`, `sfc`, or `bbox` class object.
#' @param size Size of panel border, Default: 1
#' @param color Color of panel border, Default: 'black'
#' @param bgcolor Fill color of panel background; defaults to "white". If
#'   "none", panel background is set to [ggplot2::element_blank()]
#' @param linetype Line type of panel border, Default: 'solid'
#' @param hide_grid If `TRUE`, hide grid lines. Default: `TRUE`
#' @param label_axes A description of which axes to label passed to
#'   [ggplot2::coord_sf()]; defaults to '----' which hides all axes
#'   labels.
#' @param ... Additional parameters to pass to [ggplot2::coord_sf()].
#' @inheritParams ggplot2::coord_sf
#' @inheritParams sfext::st_bbox_ext
#' @return List of [ggplot2::coord_sf] and [ggplot2::theme] calls.
#' @example examples/layer_neatline.R
#' @seealso
#'  [ggplot2::CoordSf()]
#' @family layer
#' @aliases set_map_limits
#' @export
#' @importFrom ggplot2 coord_sf scale_y_continuous scale_x_continuous theme element_rect element_blank
layer_neatline <- function(data = NULL,
                           dist = getOption("maplayer.dist"),
                           diag_ratio = getOption("maplayer.diag_ratio"),
                           unit = getOption("maplayer.unit", default = "meter"),
                           asp = getOption("maplayer.asp"),
                           crs = getOption("maplayer.crs"),
                           color = "black",
                           bgcolor = "white",
                           size = 1,
                           linetype = "solid",
                           expand = TRUE,
                           hide_grid = TRUE,
                           label_axes = "----",
                           ...) {

  xlim <- NULL
  ylim <- NULL

  if (!is.null(data)) {
    # Pass variables to bbox adjustment function
    bbox <-
      sfext::st_bbox_ext(
        x = data,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp,
        crs = crs
      )

    xlim <- c(bbox[["xmin"]], bbox[["xmax"]])
    ylim <- c(bbox[["ymin"]], bbox[["ymax"]])
  }

  # Set limits with adjustments using coord_sf
  limits <-
    list(
      ggplot2::coord_sf(
        xlim = xlim,
        ylim = ylim,
        expand = expand,
        crs = crs,
        label_axes = label_axes,
        # Suppress warning about coordinate system being displaced
        default = TRUE,
        ...
      )
    )

  if (hide_grid) {
    limits <-
      append(
        limits,
        list(
          ggplot2::theme(
            panel.grid = ggplot2::element_blank()
          )
        )
      )
  }

  if (label_axes == "----") {
    limits <-
      append(
        limits,
        list(
          ggplot2::theme(
            axis.title = ggplot2::element_blank(),
            axis.text = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            axis.ticks.length = ggplot2::unit(x = 0, units = "mm"),
            axis.line = ggplot2::element_blank()
          )
        )
      )
  }

  panel_background <- ggplot2::element_blank()
  plot_background <- ggplot2::element_blank()

  if (!is.na(bgcolor) && bgcolor != "none") {
    panel_background <- ggplot2::element_rect(fill = bgcolor, color = bgcolor)
    plot_background <- ggplot2::element_rect(fill = bgcolor, color = bgcolor)
  }

  if (is.na(color) || color == "none") {
    panel_border <- ggplot2::element_blank()
  } else {
    panel_border <-
      ggplot2::element_rect(
        color = color, size = size,
        linetype = linetype, fill = NA
      )
  }

  append(
    limits,
    list(
      ggplot2::theme(
        panel.border = panel_border,
        panel.background = panel_background,
        plot.background = plot_background
      )
    )
  )
}

#' @name set_neatline
#' @rdname layer_neatline
#' @param x For [set_neatline()], a `ggplot` class object, `ggproto` class
#'   object or list of `ggproto` objects to combine with neatline layer.
#' @param neatline A logical object, `CoordSf` object, or a list containing a
#'   `CoordSf` object (typically from [layer_neatline()]) added to layer by
#'   [set_neatline()].
#'
#'   - If logical and `TRUE`, add a neatline layer using data, crs and any
#'   additional parameters passed to ... If logical and `FALSE`, return x as is.
#'   - If object from [layer_neatline()], add it as is.
#' @export
#' @importFrom dplyr case_when
#' @importFrom rlang is_logical
#' @importFrom ggplot2 is.ggplot
set_neatline <- function(x = NULL, neatline = TRUE, data = NULL, crs = NULL, ...) {
  type <-
    dplyr::case_when(
      rlang::is_logical(neatline) && neatline ~ "lgl_true",
      rlang::is_logical(neatline) && !neatline ~ "lgl_false",
      is_neatline(neatline) ~ "coord_sf",
      TRUE ~ NA_character_
    )

  if (is.na(type)) {
    cli::cli_abort(
    c("{.arg neatline} must be {.cls logical}, a {.cls {c('Coord', 'ggproto')}} object,
      or list of {.cls {c('Coord', 'ggproto')}} objects.",
      "i" = "The class of the provided {.arg neatline} is {class(neatline)}.")
    )
  }

  neatline_layer <-
    switch(type,
      "lgl_true" = layer_neatline(
        data = data,
        crs = crs,
        ...
      ),
      "lgl_false" = x,
      "coord_sf" = neatline
    )

  if (is.null(x) | (type == "lgl_false")) {
    neatline_layer
  } else if (ggplot2::is.ggplot(x)) {
    x + neatline_layer
  } else if (is_gg(x)) {
    if (!is.list(x)) {
      x <- list(x)
    }
    c(x, neatline_layer)
  }
}
