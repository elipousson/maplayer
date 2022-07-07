#' @inheritParams layer_location_data
#' @inheritParams sfext::count_features
#' @noRd
layer_count <- function(data = NULL,
                        location = NULL,
                        boundaries = NULL,
                        mapping = NULL,
                        alpha = 0.6,
                        label_geom = "label",
                        label_col = "count",
                        count = FALSE,
                        count_col = "count",
                        basemap = FALSE,
                        crop = FALSE,
                        ...) {
  if (count) {
    # FIXME: pass data to count_features if count is TRUE
    # data <- has_same_name_col(data)
  }

  map_layer <-
    layer_location_data(
      data = data,
      geom = "sf",
      location = location,
      mapping = ggplot2::aes(fill = .data[[count_col]]),
      alpha = alpha,
      crop = crop,
      ...
    )

  # FIXME: Should I add fg_layer and bg_layer options to layer_location_data?

  # FIXME: This needs to be an option but I'm not sure how
  if (is.character(data)) {
    map_layer <-
      list(
        map_layer,
        layer_location_data(
          data = boundaries,
          location = location,
          fill = NA,
          color = "gray70",
          size = 1,
          crop = crop
        )
      )
  }

  if (!is.null(label_geom)) {
    map_layer <-
      list(
        map_layer,
        layer_location_data(
          data = data,
          geom = label_geom,
          location = location,
          label_col = label_col,
          crop = crop,
          ...
        )
      )
  }

  return(map_layer)
}
