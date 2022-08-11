#' Create a map layer showing a location in context of another area or location
#'
#' Intended for use with inset_map function.
#'
#' @param data,location data can be a sf object for the location or a ggplot
#'   layer representing the location. location can only be an sf object (not a
#'   ggplot2 layer) and it is used instead of data if both are provided.
#' @param fill Fill color for location data
#' @param color Edge color for location data
#' @param context sf or bbox object for context area or ggplot layer
#'   representing the context.
#' @param context_aes list with aesthetic attributes for context area; must
#'   include fill and color; defaults to list(fill = "white", color = "black",
#'   ...)
#' @param layer_between An optional ggplot2 layer to insert between the context
#'   layer and the location layer.
#' @param neatline If `TRUE`, add a neatline layer to the returned layer
#' @param ... Additional parameters passed to layer_location_data for location
#' @inheritParams layer_location_data
#' @name layer_location_context
#' @aliases layer_show_context
#' @export
#' @importFrom sfext is_sf
#' @importFrom sf st_crs
#' @importFrom ggplot2 theme_void
layer_location_context <- function(data = NULL,
                                   location = NULL,
                                   fill = "gray70",
                                   color = "black",
                                   context = NULL,
                                   context_aes = list(fill = "white", color = "black", alpha = 1, ...),
                                   layer_between = NULL,
                                   crs = getOption("maplayer.crs", default = 3857),
                                   neatline = TRUE,
                                   basemap = FALSE,
                                   ...) {
  if ("gg" %in% class(data)) {
    location_layer <- data
  } else if (sfext::is_sf(data) | sfext::is_sf(location)) {
    if (!is.null(location)) {
      data <- location
    }

    location_layer <-
      layer_location_data(data = data, fill = fill, color = color, crs = crs, ...)
  }

  if ("gg" %in% class(context)) {
    context_layer <- context
  } else if (sfext::is_sf(context)) {
    context_layer <-
      layer_location_data(data = context, fill = context_aes$fill, color = context_aes$color, alpha = context_aes$alpha, crs = crs)
  }

  neatline_layer <- NULL

  if (neatline && is_sf(context)) {
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
