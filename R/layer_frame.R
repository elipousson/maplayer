#' Create a frame layer around a simple feature object
#'
#' Create a circle or square that can be used as a frame around a simple feature
#' object using fixed aesthetics for fill, color, size, and linetype. This
#' function is helpful for the background of an inset map intended for use with
#' [layer_inset].
#'
#' The [make_frame] helper function calls [sfext::st_circle] (if `style =
#' "circle"`), [sfext::st_square]  (if `style = "square"`), [sfext::st_bbox_ext]
#' (if `style = "rect"`), or [sfext::st_buffer_ext] (if `style = "none"`).
#'
#' If neatline is `TRUE`, [layer_frame] returns a list of two geoms, the second
#' a [layer_neatline] layer created using the frame object as the data and the
#' parameters `bgcolor = "none"` and `color = "none"`. asp is set to 1 if style is
#' "circle" or "square" or the provided asp value otherwise.
#'
#' Additional parameters passed through ... can include additional fixed
#' aesthetics (e.g. alpha). If using the fn parameter, the function is applied
#' to the frame simple feature object created by [make_frame] (not to the
#' original input data).
#'
#' @param data,x A `sf`, `sfc`, or `bbox` object to create the frame around.
#' @param style Style of framing shape to add, "circle", "square", "rect",
#'   "buffer", or "none". If style is "buffer", the asp parameter is ignored. If
#'   style is "none", the dist, diag_ratio, and asp parameters are ignored and
#'   the input data is used as the frame.
#' @param union If `TRUE`, pass data to [sf::st_union] before buffering and
#'   creating frame; defaults to `TRUE`.
#' @inheritParams layer_neatline
#' @inheritParams sfext::st_square
#' @inheritParams sfext::st_circle
#' @param fill,color,size,linetype Fixed aesthetics for frame, passed to
#'   [layer_location_data].
#' @param neatline If `TRUE`, return a list of layers that includes a
#'   [layer_neatline]
#' @inheritDotParams layer_location_data -geom -data -package -from_crs
#' @inheritParams make_basemap
#' @example examples/layer_frame.R
#' @name layer_frame
#' @family layer
#' @export
layer_frame <- function(data = NULL,
                        dist = NULL,
                        diag_ratio = NULL,
                        unit = "meter",
                        asp = NULL,
                        style = "circle",
                        scale = 1,
                        rotate = 0,
                        inscribed = FALSE,
                        color = "black",
                        size = 1,
                        linetype = "solid",
                        fill = "white",
                        neatline = TRUE,
                        expand = TRUE,
                        basemap = FALSE,
                        union = TRUE,
                        by_feature = FALSE,
                        ...) {
  if (is.null(data)) {
    frame <- function(x) {
      make_frame(
        x = x,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        style = style,
        asp = asp,
        scale = scale,
        rotate = rotate,
        inscribed = inscribed,
        union = union,
        by_feature = by_feature
      )
    }
  } else {
    frame <- make_frame(
      x = data,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      style = style,
      asp = asp,
      scale = scale,
      rotate = rotate,
      inscribed = inscribed,
      union = union,
      by_feature = by_feature
    )
  }

  frame_layer <-
    ggplot2::geom_sf(
      data = frame,
      fill = fill,
      color = color,
      linetype = linetype,
      ...
    )

  if (is.null(data)) {
    return(frame_layer)
  }

  if (style %in% c("circle", "square")) {
    asp <- 1
  }

  frame_layer <-
    set_neatline(
      frame_layer,
      neatline = neatline,
      data = frame,
      asp = asp,
      bgcolor = "none",
      color = "none",
      expand = expand
    )

  make_basemap(
    frame_layer,
    basemap
  )
}

#' @inheritParams sfext::st_misc
#' @name make_frame
#' @rdname layer_frame
#' @param by_feature If `TRUE`, create a frame around each feature. If `FALSE`
#'   (default), union the provided features before creating a frame.
#' @inheritParams sfext::st_misc
#' @inheritParams sfext::st_buffer_ext
#' @export
#' @importFrom sf st_union
#' @importFrom sfext check_sf as_sf st_buffer_ext st_circle st_square
#' @importFrom rlang arg_match
make_frame <- function(x,
                       dist = NULL,
                       diag_ratio = NULL,
                       unit = "meter",
                       asp = NULL,
                       style = "circle",
                       scale = 1,
                       rotate = 0,
                       inscribed = FALSE,
                       dTolerance = 0,
                       union = TRUE,
                       by_feature = FALSE) {
  style <-
    rlang::arg_match(style, c("circle", "square", "rect", "buffer", "none"))

  if (style == "none") {
    if (!all(vapply(c(dist, diag_ratio, asp), is.null, FALSE))) {
      cli_warn(
        "Provided {.arg dist}, {.arg diag_ratio}, and {.arg asp} are ignored
        when {.code style = none}."
      )
    }
    return(x)
  }

  if (union) {
    x <- sf::st_union(x)
  }

  if (is.null(asp) | (style == "buffer")) {
    x <-
      sfext::st_buffer_ext(
        x = x,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit
      )
  } else {
    x <-
      st_bbox_ext(
        x = x,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp,
        class = "sf"
      )
  }

  switch(style,
    "circle" = sfext::st_circle(x, scale = scale, inscribed = inscribed, by_feature = by_feature, dTolerance = dTolerance),
    "square" = sfext::st_square(x, scale = scale, rotate = rotate, by_feature = by_feature, inscribed = inscribed),
    "rect" = st_bbox_ext(x, asp = asp, class = "sf"),
    "buffer" = x
  )
}
