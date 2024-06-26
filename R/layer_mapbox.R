#' Use mapboxapi to make a Mapbox static map layer
#'
#' @param data A `sf`, `sfc`, or `bbox` object.
#' @inheritParams layer_neatline
#' @inheritParams mapboxapi::layer_static_mapbox
#' @param style_url Map style url used to fill style_id and username parameters,
#'   Default: "mapbox://styles/mapbox/satellite-streets-v11"
#' @param basemap If FALSE, create a standalone layer; if `TRUE`, the layer is
#'   preceded by [ggplot2::ggplot()] to allow use as a basemap, Default:
#'   `TRUE`
#' @param neatline If `TRUE`, add a neatline matching the provided data,
#'   Default: `TRUE`
#' @inheritParams layer_neatline
#' @param ... Additional parameter passed to [mapboxapi::layer_static_mapbox]
#' @seealso
#' [mapboxapi::layer_static_mapbox()]
#' @rdname layer_mapbox
#' @md
#' @export
layer_mapbox <- function(data = NULL,
                         dist = NULL,
                         diag_ratio = NULL,
                         unit = "meter",
                         asp = NULL,
                         style_url = "mapbox://styles/mapbox/satellite-streets-v11",
                         style_id = NULL,
                         username = NULL,
                         basemap = FALSE,
                         scale = 0.75,
                         scaling_factor = "1x",
                         attribution = TRUE,
                         logo = TRUE,
                         access_token = NULL,
                         neatline = TRUE,
                         color = "black",
                         bgcolor = "white",
                         linewidth = 0.5,
                         linetype = "solid",
                         expand = TRUE,
                         hide_grid = TRUE,
                         label_axes = "----",
                         ...) {
  check_dev_installed("mapboxapi", repo = "walkerke/mapboxapi")

  # Set appropriate CRS for Mapbox
  crs_mapbox <- 3857

  bbox <- st_bbox_ext(
    x = data,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    asp = asp,
    crs = crs_mapbox
  )

  # Get Mapbox map
  mapbox_layer <- mapboxapi::layer_static_mapbox(
    location = bbox,
    buffer_dist = 0,
    style_url = style_url,
    style_id = style_id,
    username = username,
    scale = scale,
    scaling_factor = scaling_factor,
    attribution = attribution,
    logo = logo,
    access_token = access_token,
    ...
  )

  if (!is_false(basemap)) {
    mapbox_layer <- set_basemap(mapbox_layer, basemap)
  }

  if (is_false(neatline)) {
    return(mapbox_layer)
  }

  set_neatline(
    mapbox_layer,
    neatline = neatline,
    data = bbox,
    crs = crs_mapbox,
    color = color,
    bgcolor = bgcolor,
    linewidth = linewidth,
    linetype = linetype,
    expand = expand,
    hide_grid = hide_grid,
    label_axes = label_axes
  )
}
