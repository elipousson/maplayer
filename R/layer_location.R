#' Layer a location border into a ggplot2 map
#'
#' Helper function to make a ggplot2 layer from data returned by
#' `get_location`
#'
#' @param data Data for location to show.
#' @param label label type (e.g. "text", "label")
#' @param color Color for location; defaults to "black".
#' @param linewidth Line width for location; defaults to 0.5.
#' @param linetype Line type for location; defaults to "dashed".
#' @param fill Fill for location; defaults to "NA".
#' @param label_geom Optional character string or function with geom to use for
#'   labelling location layer. Passed to geom parameter of [layer_labelled()]
#' @param ... Additional parameters passed to get_location if data is `NULL`.
#' @inheritParams getdata::get_location
#' @inheritParams layer_location_data
#' @inheritParams layer_mask
#' @return list of ggplot2 geoms
#' @seealso
#'  [ggplot2::CoordSf()]
#' @rdname layer_location
#' @aliases layer_show_location
#' @export
#' @importFrom ggplot2 aes
#' @importFrom getdata get_location
layer_location <-
  function(mapping = ggplot2::aes(),
           data = NULL,
           type = NULL,
           name = NULL,
           id = NULL,
           location = NULL,
           name_col = "name",
           id_col = "id",
           index = NULL,
           label = NULL,
           label_geom = NULL,
           label_col = name_col,
           union = FALSE,
           crs = getOption("maplayer.crs", default = 3857),
           color = "gray40",
           linewidth = 0.5,
           linetype = "dashed",
           fill = NA,
           dist = NULL,
           diag_ratio = NULL,
           unit = NULL,
           asp = NULL,
           mask = FALSE,
           neatline = FALSE,
           smooth_params = NULL,
           shadow_params = NULL,
           ...) {
    if (is.null(data) && (!is.null(type) | !is.null(index))) {
      data <-
        getdata::get_location(
          type = type,
          name = name,
          id = id,
          location = location,
          label = label,
          name_col = name_col,
          id_col = id_col,
          index = index,
          union = union,
          ...
        )
    }

    location_layer <-
      layer_location_data(
        data = data,
        color = color,
        linewidth = linewidth,
        linetype = linetype,
        fill = fill
      )

    if (!is.null(label_geom)) {
      # label_geom <- match.arg(label_geom, c("label", "text"))

      label_layer <-
        layer_labelled(
          data = data,
          color = "black",
          geom = label_geom,
          label_col = label_col
        )

      location_layer <-
        c(
          location_layer,
          label_layer
        )
    }

    location_layer <-
      set_mask(
        x = location_layer,
        mask = mask,
        data = data,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp,
        crs = crs
      )

    set_neatline(
      x = location_layer,
      neatline = neatline,
      data = data,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp,
      crs = crs
    )
  }
