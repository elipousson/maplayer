#' Create map layer with shape framing a simple feature object
#'
#' Wraps [overedge::st_circle], [overedge::st_square], and [layer_neatline].
#'
#' @param frame Type of framing shape to add, "circle" or "square" around data.
#' @param union If `TRUE`, union data before buffering and creating frame;
#'   defaults to `TRUE`.
#' @inheritParams overedge::layer_neatline
#' @inheritParams overedge::st_misc
#' @inheritParams st_buffer_ext
#' @param fill Fill color for frame.
#' @param neatline If TRUE, add a neatline to the returned layer.
#' @param ... Additional parameters passed to [overedge::layer_location_data]. May include
#'   additional fixed aesthetics (e.g. alpha) or "fn" to apply to the frame
#'   object.
#' @example examples/layer_frame.R
#' @name layer_frame
#' @family layer
#' @export
layer_frame <- function(data,
                        dist = NULL,
                        diag_ratio = NULL,
                        unit = "meter",
                        frame = "circle",
                        scale = 1,
                        rotate = 0,
                        inscribed = FALSE,
                        color = "black",
                        size = 1,
                        linetype = "solid",
                        fill = "white",
                        neatline = TRUE,
                        expand = FALSE,
                        union = TRUE,
                        ...) {
  if (union) {
    data <- sf::st_union(data)
  }

  data <-
    st_buffer_ext(
      x = data,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit
    )

  frame <-
    make_frame(x = data, frame = frame, scale = scale, rotate = rotate, inscribed = inscribed)

  frame_layer <-
    overedge::layer_location_data(
      data = frame,
      geom = "sf",
      fill = fill,
      color = color,
      linetype = linetype,
      ...
    )

  if (neatline) {
    neatline_layer <-
      overedge::layer_neatline(
        data = frame,
        asp = 1,
        bgcolor = "none",
        color = "none",
        expand = expand
      )

    frame_layer <-
      list(
        frame_layer,
        neatline_layer
      )
  }

  return(frame_layer)
}

#' @inheritParams overedge::st_misc
#' @name make_frame
#' @rdname layer_frame
#' @export
make_frame <- function(x,
                       frame = "circle",
                       scale = 1,
                       rotate = 0,
                       inscribed = FALSE,
                       dTolerance = 0) {
  stopifnot(
    overedge::is_sf(x, ext = TRUE)
  )

  frame <- match.arg(frame, c("circle", "square", "other"))

  frame <-
    switch(frame,
      "circle" = overedge::st_circle(x = x, scale = scale, inscribed = inscribed, dTolerance = dTolerance),
      "square" = overedge::st_square(x = x, scale = scale, rotate = rotate, inscribed = inscribed),
      "other" = x
    )
  return(frame)
}
