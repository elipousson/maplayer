#' Make a map using layer_mapbox
#'
#' Wraps [make_layer_map()] and passes layer created with [layer_mapbox()] to
#' bg_layer and the neatline parameters to [layer_neatline()] (using the same
#' data as the mapbox background layer). The neatline parameters are only used
#' if neatline is NULL.
#'
#' @inheritParams layer_mapbox
#' @inheritParams layer_neatline
#' @param crs Must be in web mercator projection.
#' @inheritDotParams make_layer_map -bg_layer
#' @keywords internal
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
                            # width = NULL,
                            # height = NULL,
                            # units = NULL,
                            # orientation = NULL,
                            crs = 3857, # Do not change this
                            ...) {
  if (!is_same_crs(as_crs(crs), "EPSG:3857")) {
    cli_abort(
      "{.arg crs} must be a Web Mercator projection ('EPSG:3857')."
    )
  }
  # page <-
  #   papersize::make_page_size(
  #     width = width,
  #     height = height,
  #     units = units,
  #     orientation = orientation
  #   )

  make_layer_map(
    bg_layer = layer_mapbox(
      data = data,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp, # %||% page$asp,
      style_url = style_url,
      style_id = style_id,
      username = username,
      basemap = basemap,
      scale = scale,
      neatline = FALSE,
      scaling_factor = scaling_factor,
      attribution = attribution,
      logo = logo,
      access_token = access_token
    ),
    crs = crs,
    ...,
    basemap = basemap,
    neatline = neatline %||%
      layer_neatline(
        data = data,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp, # %||% page$asp,
        color = color,
        bgcolor = bgcolor,
        linewidth = linewidth,
        linetype = linetype,
        expand = expand,
        hide_grid = hide_grid,
        label_axes = label_axes
      )
  )
}
