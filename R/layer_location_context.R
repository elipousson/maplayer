#' Create a map layer showing a location in context of another area or location
#'
#' Intended for use with inset_map function.
#'
#' @param data sf object with location data
#' @param fill Fill color for location data
#' @param color Edge color for location data
#' @param context sf or bbox object for context area or a `geom` layer
#' @param context_aes list with aesthetic attributes for context area; must
#'   include fill and color; defaults to list(fill = "white", color = "black",
#'   ...)
#' @param layer_between An optional ggplot2 layer to insert between the context
#'   layer and the location layer.
#' @param neatline If `TRUE`, add a neatline layer to the returned layer
#' @param ... Additional parameters passed to layer_location_data for
#'   location
#' @inheritParams layer_location_data
#' @name layer_location_context
#' @aliases layer_show_context
#' @export
#' @importFrom overedge is_sf layer_location_data layer_neatline
#' @importFrom sf st_crs
#' @importFrom ggplot2 theme_void
layer_location_context <- function(data = NULL,
                                   fill = "gray70",
                                   color = "black",
                                   context = NULL,
                                   context_aes = list(fill = "white", color = "black", alpha = 1, ...),
                                   layer_between = NULL,
                                   crs = NULL,
                                   neatline = TRUE,
                                   ...) {
  if (sfext::is_sf(data)) {
    # FIXME: Is this crs check actually required?
    if (is.null(crs)) {
      crs <- sf::st_crs(data)
    }

    location_layer <-
      layer_location_data(data = data, fill = fill, color = color, crs = crs, ...)
  } else if ("gg" %in% class(data)) {
    location_layer <- data
  }

  is_context <- sfext::is_sf(context)

  if (is_context) {
    context_layer <-
      layer_location_data(data = context, fill = context_aes$fill, color = context_aes$color, alpha = context_aes$alpha, crs = crs)
  } else if ("gg" %in% class(context)) {
    context_layer <- context
  }

  neatline_layer <- NULL

  if (neatline && is_context) {
    neatline_layer <-
      list(
        layer_neatline(
          data = context,
          color = NA,
          bgcolor = "none",
          crs = crs
        ),
        # FIXME: Adding theme_void here is heavy-handed
        ggplot2::theme_void()
      )
  }

  list(
    context_layer,
    layer_between,
    location_layer,
    neatline_layer
  )
}
