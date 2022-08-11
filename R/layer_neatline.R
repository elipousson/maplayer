#' Set map limits to a feature or bounding box with a buffer, set aspect ratio,
#' and panel border
#'
#' Set limits for a map to the bounding box of a feature using [ggplot2::coord_sf()].
#' Optionally, adjust the x size by applying a buffer and/or adjust the aspect
#' ratio of the limiting bounding box to match a set aspect ratio.
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

  # Set limits with adjustments using coord_sf
  limits <-
    list(
      ggplot2::coord_sf(
        xlim = c(bbox[["xmin"]], bbox[["xmax"]]),
        ylim = c(bbox[["ymin"]], bbox[["ymax"]]),
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

  if (is.na(bgcolor) || bgcolor == "none") {
    panel_background <- ggplot2::element_blank()
    plot_background <- ggplot2::element_blank()
  } else {
    panel_background <- ggplot2::element_rect(fill = bgcolor)
    plot_background <- ggplot2::element_rect(fill = bgcolor)
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
