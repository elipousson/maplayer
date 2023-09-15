#' Make a map using layer_mapbox
#'
#' Wraps [make_layer_map()] and passes layer created with [layer_mapbox()] to
#' basemap and the neatline parameters to [layer_neatline()] (using the same
#' data as the Mapbox background layer). The neatline parameters are only used
#' if neatline is `NULL`.
#'
#' @inheritParams layer_mapbox
#' @param location If `location` is provided and `data` is `NULL`, `location` is
#'   used in place of `data`.
#' @inheritParams layer_neatline
#' @inheritParams papersize::make_page_size
#' @inheritDotParams make_layer_map
#' @export
make_mapbox_map <- function(data = NULL,
                            dist = NULL,
                            diag_ratio = NULL,
                            unit = "meter",
                            asp = NULL,
                            style_url = "mapbox://styles/mapbox/satellite-streets-v11",
                            style_id = NULL,
                            username = NULL,
                            basemap = TRUE,
                            scale = 0.75,
                            scaling_factor = "1x",
                            attribution = TRUE,
                            logo = TRUE,
                            access_token = NULL,
                            neatline = NULL,
                            color = "black",
                            bgcolor = "white",
                            linewidth = 0.5,
                            linetype = "solid",
                            expand = TRUE,
                            hide_grid = TRUE,
                            label_axes = "----",
                            width = NULL,
                            height = NULL,
                            units = NULL,
                            orientation = NULL,
                            location = NULL,
                            ...) {
  if (!is.null(location) && is.null(data)) {
    data <- location
  }

  page <- list(asp = asp)

  if (!is.null(units)) {
    page <- papersize::make_page_size(
      width = width,
      height = height,
      units = units,
      orientation = orientation
    )
  }

  make_layer_map(
    basemap = layer_mapbox(
      basemap = TRUE,
      neatline = FALSE,
      data = data,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp %||% page[["asp"]],
      style_url = style_url,
      style_id = style_id,
      username = username,
      scale = scale,
      scaling_factor = scaling_factor,
      attribution = attribution,
      logo = logo,
      access_token = access_token
    ),
    crs = 3857,
    neatline = neatline %||%
      layer_neatline(
        data = data,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp %||% page[["asp"]],
        color = color,
        bgcolor = bgcolor,
        linewidth = linewidth,
        linetype = linetype,
        expand = expand,
        hide_grid = hide_grid,
        label_axes = label_axes,
        crs = 3857
      ),
    ...
  )
}
