#' Create a layer showing a location and related context
#'
#' Create a ggplot2 layer for a location in a provided context. This function is
#' useful for making inset locator maps in combination with the [make_inset_map]
#' function.
#'
#' @param data,location A data and location can be a either `sf` object or a
#'   ggplot layer. Either data or location is required and, if data and location
#'   are both provided, location is ignored. If data is `NULL` and location is a
#'   formula or function, the context data is passed to the location function
#'   and the results used as the data for the location layer.
#' @param fill,color Fill and color fixed aesthetics for location data.
#' @param context A `sf` object for context area or a ggplot layer representing
#'   the context.
#' @param context_params A list with parameters for context layer; defaults to
#'   `list(fill = "white", color = "black", alpha = 1, ...)`.
#' @param mid_layer A ggplot2 layer to insert between the context
#'   layer and the location layer. Optional.
#' @param neatline A logical object, `CoordSf` object, or a list containing a
#'   `CoordSf` object (typically from [layer_neatline()]) added to layer.
#'
#'   - If logical and `TRUE`, add a neatline layer using data from the context
#'   layer with `color = NA` and `bgcolor = "none"`.
#'   - If object from [layer_neatline()], add it as is.
#' @param ... Additional parameters passed to [layer_location_data()] if data or
#'   location is an `sf` object.
#' @inheritParams layer_location_data
#' @name layer_location_context
#' @aliases layer_show_context
#' @export
#' @importFrom dplyr case_when
layer_location_context <- function(data = NULL,
                                   location = NULL,
                                   fill = "gray70",
                                   color = "black",
                                   context = NULL,
                                   context_params = list(
                                     fill = "white",
                                     color = "black",
                                     alpha = 1,
                                     ...
                                   ),
                                   crs = getOption("maplayer.crs", default = 3857),
                                   mid_layer = NULL,
                                   neatline = TRUE,
                                   basemap = FALSE,
                                   ...) {
  location_type <-
    dplyr::case_when(
      is_fn(location) && is.null(data) ~ "fn_location",
      is_sf(data) | is_sf(location) ~ "sf",
      obj_is_gg(data) | obj_is_gg(location) ~ "gg"
    )

  context_type <-
    dplyr::case_when(
      is_sf(context) ~ "sf",
      obj_is_gg(context) ~ "gg"
    )

  if ((location_type == "fn_location") && (context_type == "sf")) {
    if (context_type == "gg") {
      context_data <- context_layer[[1]]$data
      sfext::check_sf(context_data)
    } else {
      context_data <- context
    }

    data <- use_fn(context_data, location)
    location_type <- "sf"
  }

  location_layer <-
    switch(location_type,
      "sf" = layer_location_data(
        data = data %||% location,
        fill = fill,
        color = color,
        crs = crs,
        ...
      ),
      "gg" = data %||% location
    )

  if (is.null(context_params$mapping)) {
    context_params$mapping <- aes()
  }

  context_layer <-
    switch(context_type,
      "sf" = eval_tidy_fn(
        context,
        fn = layer_location_data,
        params = context_params
      ),
      "gg" = context
    )

  if (!is.null(mid_layer)) {
    context_layer <-
      c(
        context_layer,
        mid_layer
      )
  }

  context_layer <-
    c(
      context_layer,
      location_layer
    )

  context_layer <-
    set_neatline(
      x = context_layer,
      neatline = neatline,
      data = context_layer[[1]]$data,
      color = NA,
      bgcolor = "none",
      crs = crs
    )

  set_basemap(context_layer, basemap)
}
