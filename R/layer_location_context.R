#' Create a layer showing a location and related context
#'
#' Intended for use with [make_inset_map] function.
#'
#' @param data,location data and location can be a either sf object for the
#'   location or a ggplot layer representing the location. location can also be
#'   a formula or a function used to subset context (requires context to be an
#'   sf object). If data and context are both provided and both are sf objects,
#'   data is ignored.
#' @param fill,color Fill and color aesthetics for location data
#' @param context A sf object for context area or a ggplot layer representing
#'   the context.
#' @param context_params list with parameters for context layer; defaults to
#'   list(fill = "white", color = "black", alpha = 1, ...).
#' @param layer_between An optional ggplot2 layer to insert between the context
#'   layer and the location layer.
#' @param neatline If `TRUE`, add a neatline layer to the returned layer.
#'   Requires that data be an sf object and not inherited.
#' @param ... Additional parameters passed to [layer_location_data] for location
#' @inheritParams layer_location_data
#' @name layer_location_context
#' @aliases layer_show_context
#' @export
#' @importFrom sfext is_sf
layer_location_context <- function(data = NULL,
                                   location = NULL,
                                   fill = "gray70",
                                   color = "black",
                                   context = NULL,
                                   context_params = list(fill = "white", color = "black", alpha = 1, ...),
                                   layer_between = NULL,
                                   crs = getOption("maplayer.crs", default = 3857),
                                   neatline = TRUE,
                                   basemap = FALSE,
                                   ...) {
  sf_context <- sfext::is_sf(context)

  if (is_function(location) && sf_context) {
    data <- location(context)
  } else if (is_formula(location) && sf_context) {
    data <- use_fn(context, location)
  }

  if (sfext::is_sf(data) | sfext::is_sf(location)) {
    if (!is.null(location)) {
      data <- location
    }

    location_layer <-
      layer_location_data(data = data, fill = fill, color = color, crs = crs, ...)
  } else if ("gg" %in% class(data)) {
    location_layer <- data
  } else if ("gg" %in% class(location)) {
    location_layer <- location
  }

  if (sf_context) {
    context_layer <-
      eval_tidy(quo(layer_location_data(data = context, !!!context_params)))
  } else if ("gg" %in% class(context)) {
    context_layer <- context
  }

  neatline_layer <- NULL

  if (neatline && sf_context) {
    neatline_layer <-
      layer_neatline(
        data = context,
        color = NA,
        bgcolor = "none",
        crs = crs
      )
  }

  layer_stack <-
    list(
      context_layer,
      layer_between,
      location_layer,
      neatline_layer
    )

  make_basemap(layer_stack, basemap)
}
