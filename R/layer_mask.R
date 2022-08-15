#' Create a mask layer based on a simple feature object
#'
#' Returns a mask for an area or areas as a simple feature object.
#'
#' @param data `sf`, `sfc`, or `bbox` object. If dist, diag_ratio, and/or asp
#'   are provided, data is adjusted to set the boundaries of the mask. If data
#'   is not provided, `mask` is required.
#' @inheritParams sfext::st_bbox_ext
#' @param fill mask fill color; defaults to "white"
#' @param color mask edge color; defaults to `NA`
#' @param alpha mask alpha/transparency; defaults to 0.5
#' @param mask A `sf` or `bbox` object to define the edge of the mask.
#'   `diag_ratio`, `dist`, and `asp` parameters are ignored if a `mask` is
#'   provided. defaults to `NULL`
#' @inheritParams layer_neatline
#' @param ... Additional parameters to pass to [ggplot2::geom_sf()]
#' @return  [ggplot2::geom_sf()] function.
#' @export
#' @importFrom sf st_transform st_difference st_union
#' @importFrom sfext as_sf st_transform_ext st_erase
layer_mask <- function(data = NULL,
                       dist = NULL,
                       diag_ratio = NULL,
                       unit = NULL,
                       asp = NULL,
                       crs = getOption("maplayer.crs", default = 3857),
                       fill = "white",
                       color = NA,
                       alpha = 0.5,
                       mask = NULL,
                       neatline = FALSE,
                       expand = TRUE,
                       ...) {
  # Check if mask is provided
  if (is.null(mask)) {
    # Get adjusted bbox
    mask <-
      st_bbox_ext(
        x = data,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp,
        crs = crs,
        class = "sf"
      )
  } else {
    # Convert mask to sf if needed
    mask <- sfext::as_sf(mask)
  }

  if (!is.null(data)) {
    # Erase data from mask
    data <- sfext::st_transform_ext(data, crs, class = "sf")
    mask <- sfext::st_erase(x = mask, y = data)
  }

  # Create mask layer
  mask_layer <-
    layer_location_data(
      data = mask,
      geom = "sf",
      fill = fill,
      color = color,
      alpha = alpha,
      ...
    )

  if (!neatline) {
    return(mask_layer)
  }

  if (all(vapply(c(dist, diag_ratio, asp), is.null, TRUE))) {
    neatline_data <- mask
  } else {
    neatline_data <- data
  }

  set_neatline(
    mask_layer,
    data = neatline_data,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    asp = asp,
    crs = crs,
    expand = expand
  )
}

#' @noRd
set_mask <- function(x = NULL, mask = TRUE, data = NULL, crs = NULL, ...) {
  type <-
    dplyr::case_when(
      is_sf(mask, ext = TRUE) ~ "sf",
      rlang::is_logical(mask) && mask && !is.null(data) ~ "lgl_true",
      rlang::is_logical(mask) && !mask ~ "lgl_false",
      is_gg(mask) ~ "gg",
      TRUE ~ NA_character_
    )

  if (is.na(type)) {
    cli::cli_abort(
      c("{.arg mask} must be sf, logical, or ggproto object.",
        "i" = "The class of the provided {.arg mask} is {class(mask)}.")
    )
  }

  mask_layer <-
    switch(type,
      "lgl_true" = layer_mask(
        data = data,
        crs = crs,
        ...
      ),
      "lgl_false" = x,
      "sf" = layer_mask(
        data = mask,
        crs = crs,
        ...
      ),
      "gg" = mask
    )

  if (is.null(x) | (type == "lgl_false")) {
    mask_layer
  } else if (ggplot2::is.ggplot(x)) {
    x + mask_layer
  } else if (is_gg(x)) {
    c(
      x,
      mask_layer
    )
  }
}
