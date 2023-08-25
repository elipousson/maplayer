#' Create a mask layer based on a simple feature object
#'
#' Returns a mask for an area or areas as a simple feature object. `neatline =
#' TRUE` only works for this layer if data is passed directly; not inherited.
#'
#' @param data `sf`, `sfc`, or `bbox` object. If dist, diag_ratio, and/or asp
#'   are provided, data is adjusted to set the boundaries of the mask. If data
#'   is not provided, `mask` is required. If data is `NA`, mask is continuous
#'   otherwise the data is erased from the mask area.
#' @inheritParams sfext::st_bbox_ext
#' @param fill mask fill color; defaults to "white"
#' @param color mask edge color; defaults to `NA`
#' @param alpha mask alpha/transparency; defaults to 0.5
#' @param mask A `sf`, `sfc`, or `bbox` object to define the mask area.
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
  mask_layer <- ggplot2::layer_sf(
    geom = ggplot2::GeomSf,
    stat = "sf",
    data = make_mask_data(
      data,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp,
      crs = crs,
      mask = mask
    ),
    position = "identity",
    params = list(
      fill = fill,
      color = color,
      alpha = alpha,
      ...
    )
  )

  if (!neatline) {
    return(mask_layer)
  }

  set_mask_neatline(
    mask_layer,
    data = data,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    asp = asp,
    crs = crs,
    mask = mask,
    expand = expand
  )
}

#' @noRd
set_mask_neatline <- function(mask_layer,
                              data = NULL,
                              dist = NULL,
                              diag_ratio = NULL,
                              unit = NULL,
                              asp = NULL,
                              crs = getOption("maplayer.crs", default = 3857),
                              mask = NULL,
                              expand = TRUE) {
  if (all(vapply(c(dist, diag_ratio, asp), is.null, TRUE)) && !is.null(mask)) {
    set_neatline(
      mask_layer,
      data = mask,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp,
      crs = crs,
      expand = expand
    )
  } else if (!is.null(data)) {
    set_neatline(
      mask_layer,
      data = data,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp,
      crs = crs,
      expand = expand
    )
  }
}

#' Make mask data to pass to layer_sf
#'
#' @noRd
#' @importFrom sfext check_sf st_erase
#' @importFrom cliExtras cli_warn_ifnot
make_mask_data <- function(data = NULL,
                           dist = NULL,
                           diag_ratio = NULL,
                           unit = NULL,
                           asp = NULL,
                           crs = getOption("maplayer.crs", default = 3857),
                           mask = NULL) {
  if (is.null(data)) {
    return(
      function(x) {
        make_mask_data(
          x,
          dist = dist,
          diag_ratio = diag_ratio,
          unit = unit,
          asp = asp,
          crs = crs,
          mask = mask
        )
      }
    )
  }

  if (!is.null(mask)) {
    sfext::check_sf(mask, allow_null = TRUE, ext = TRUE)
    cliExtras::cli_warn_ifnot(
      "{.arg {c('dist', 'diag_ratio', 'asp')}} are ignored when
      {.arg mask} is provided.",
      condition = is_all_null(c(dist, diag_ratio, asp))
    )
  }

  # Get adjusted bbox if mask is not provided
  mask <- mask %||%
    st_bbox_ext(
      x = data,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp,
      crs = crs,
      class = "sf"
    )

  if (all(is.na(data))) {
    return(mask)
  }

  sfext::st_erase(x = mask, y = data)
}

#' @name set_mask
#' @rdname layer_mask
#' @export
#' @importFrom dplyr case_when
#' @importFrom rlang is_logical
#' @importFrom cli cli_abort
#' @importFrom ggplot2 is.ggplot
set_mask <- function(x = NULL, mask = TRUE, data = NULL, crs = NULL, ...) {
  type <- dplyr::case_when(
    rlang::is_logical(mask) && mask && !is.null(data) ~ "lgl_true",
    rlang::is_logical(mask) && !mask ~ "lgl_false",
    is_sf(mask, ext = TRUE) ~ "sf",
    obj_is_gg(mask) ~ "gg",
    TRUE ~ NA_character_
  )

  cliExtras::cli_abort_ifnot(
    c("{.arg mask} must be sf, logical, or ggproto object.",
      "i" = "The class of the provided {.arg mask} is {class(mask)}."
    ),
    condition = !is.na(type)
  )

  mask_layer <- switch(type,
    "lgl_false" = x,
    "lgl_true" = layer_mask(
      data = data,
      crs = crs,
      ...
    ),
    "sf" = layer_mask(
      data = mask,
      crs = crs,
      ...
    ),
    "gg" = mask
  )

  if (is.null(x) || (type == "lgl_false")) {
    return(mask_layer)
  }

  combine_gg_list(x, mask_layer)
}
