#' Set map limits to a bounding box with a buffer and set aspect ratio
#'
#' Set limits for a map to the bounding box of a feature using
#' [ggplot2::coord_sf()]. Optionally, adjust the x size by applying a buffer
#' and/or adjust the aspect ratio of the limiting bounding box to match a set
#' aspect ratio.
#'
#' @param data A `sf`, `sfc`, or `bbox` class object.
#' @param linewidth Line width of panel border, Default: 0.5
#' @param color Color of panel border, Default: 'black'
#' @param bgcolor Fill color of panel background; defaults to "white". If
#'   "none", panel background is set to [ggplot2::element_blank()]
#' @param linetype Line type of panel border, Default: 'solid'
#' @param hide_grid If `TRUE`, hide grid lines. Default: `TRUE`
#' @param label_axes A description of which axes to label passed to
#'   [ggplot2::coord_sf()]; defaults to '----' which hides all axes labels.
#' @param default_margin Plot margin to use as default `ggplot2::margin(0, 0, 0,
#'   0)` if `expand = FALSE`.
#' @param ... Additional parameters passed to [ggplot2::coord_sf()].
#' @param axis.title,axis.text,axis.ticks,axis.ticks.length,axis.line Theme
#'   elements passed as is if label_axes is anything other than "----".
#' @param panel.grid,panel.grid.major,panel.grid.minor Passed as is if hide_grid
#'   is FALSE.
#' @param panel.border,panel.background,plot.background,plot.margin panel.border
#'   is used as is if not NULL or `ggplot2::element_blank()` if it is NULL
#'   unless color is NA or "none". panel.background and plot.background are used
#'   as is or `ggplot2::element_blank()` if bg color is NA or "none".
#'   plot.margin is set to `ggplot2::margin(1, 1, 1, 1)` if `NULL` or
#'   `ggplot2::margin(0, 0, 0, 0)` if expand is `FALSE`.
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
                           linewidth = 0.5,
                           linetype = "solid",
                           bgcolor = "white",
                           expand = TRUE,
                           hide_grid = TRUE,
                           label_axes = "----",
                           axis.title = NULL,
                           axis.text = NULL,
                           axis.ticks = NULL,
                           axis.ticks.length = ggplot2::unit(x = 0, units = "mm"),
                           axis.line = NULL,
                           panel.grid = NULL,
                           panel.grid.major = NULL,
                           panel.grid.minor = NULL,
                           panel.border = NULL,
                           panel.background = NULL,
                           plot.background = NULL,
                           plot.margin = NULL,
                           ...) {
  xy_lims <- set_xy_lims(data, dist, diag_ratio, unit, asp, crs)

  list(
    # Set limits with adjustments using coord_sf
    ggplot2::coord_sf(
      xlim = xy_lims$xlim,
      ylim = xy_lims$ylim,
      expand = expand,
      crs = crs,
      label_axes = label_axes,
      # Suppress warning about coordinate system being replaced
      default = TRUE,
      ...
    ),
    theme_grid(
      hide_grid,
      panel.grid = panel.grid,
      panel.grid.major = panel.grid.major,
      panel.grid.minor = panel.grid.minor
    ),
    theme_sf_axes(
      label_axes,
      axis.title = axis.title,
      axis.text = axis.text,
      axis.ticks = axis.ticks,
      axis.ticks.length = axis.ticks.length,
      axis.line = axis.line
    ),
    theme_background(
      color,
      linewidth,
      linetype,
      bgcolor,
      expand,
      panel.border = panel.border,
      panel.background = panel.background,
      plot.background = plot.background,
      plot.margin = plot.margin
    )
  )
}

#' Set xlim and ylim based on data
#'
#' @noRd
#' @importFrom sfext st_bbox_ext
set_xy_lims <- function(data = NULL,
                        dist = getOption("maplayer.dist"),
                        diag_ratio = getOption("maplayer.diag_ratio"),
                        unit = getOption("maplayer.unit", default = "meter"),
                        asp = getOption("maplayer.asp"),
                        crs = getOption("maplayer.crs")) {
  xlim <- NULL
  ylim <- NULL

  if (!is.null(data)) {
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

  list(
    "xlim" = xlim,
    "ylim" = ylim
  )
}

#' Set ggplot2 panel grid theme elements
#'
#' @importFrom ggplot2 theme element_blank
#' @noRd
theme_grid <- function(hide_grid = TRUE,
                       panel.grid = NULL,
                       panel.grid.major = NULL,
                       panel.grid.minor = NULL) {
  if (hide_grid) {
    return(
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
    )
  }

  ggplot2::theme(
    panel.grid = panel.grid,
    panel.grid.major = panel.grid.major,
    panel.grid.minor = panel.grid.minor
  )
}

#' Set ggplot2 axis theme elements based on label_axes parameter
#'
#' @noRd
#' @importFrom ggplot2 theme element_blank unit
theme_sf_axes <- function(label_axes = "----",
                          axis.title = NULL,
                          axis.text = NULL,
                          axis.ticks = NULL,
                          axis.ticks.length = ggplot2::unit(x = 0, units = "mm"),
                          axis.line = NULL) {
  if (label_axes == "----") {
    return(
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.ticks.length = ggplot2::unit(x = 0, units = "mm"),
        axis.line = ggplot2::element_blank()
      )
    )
  }

  ggplot2::theme(
    axis.title = axis.title,
    axis.text = axis.text,
    axis.ticks = axis.ticks,
    axis.ticks.length = axis.ticks.length,
    axis.line = axis.line
  )
}

#' Set ggplot2 panel border, panel background, and plot background theme
#' elements
#'
#' @noRd
#' @importFrom ggplot2 theme element_blank element_rect
theme_background <- function(color = "black",
                             linewidth = 0.5,
                             linetype = "solid",
                             bgcolor = "white",
                             expand = TRUE,
                             plot.background = NULL,
                             plot.margin = NULL,
                             panel.border = NULL,
                             panel.background = NULL) {
  panel.border <- panel.border %||% ggplot2::element_blank()
  panel.background <- panel.background %||% ggplot2::element_blank()
  plot.background <- plot.background %||% ggplot2::element_blank()
  plot.margin <- plot.margin %||% ggplot2::margin(1, 1, 1, 1)

  if (!is.na(color) && (color != "none")) {
    panel.border <-
      ggplot2::element_rect(
        color = color, linewidth = linewidth,
        linetype = linetype, fill = NA
      )
  }

  if (!is.na(bgcolor) && bgcolor != "none") {
    panel.background <- ggplot2::element_rect(fill = bgcolor, color = bgcolor)
    plot.background <- ggplot2::element_rect(fill = bgcolor, color = bgcolor)
  }

  if (!expand) {
    plot.margin <- ggplot2::margin(0, 0, 0, 0)
  }

  ggplot2::theme(
    panel.border = panel.border,
    panel.background = panel.background,
    plot.background = plot.background,
    plot.margin = plot.margin
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
set_neatline <- function(x = NULL,
                         neatline = TRUE,
                         data = NULL,
                         crs = NULL,
                         ...) {
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
        "i" = "The class of the provided {.arg neatline} is {class(neatline)}."
      )
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
    return(neatline_layer)
  }

  if (ggplot2::is.ggplot(x)) {
    return(x + neatline_layer)
  }

  if (is_gg(x)) {
    if (!is.list(x)) {
      x <- list(x)
    }
    return(c(x, neatline_layer))
  }
}
