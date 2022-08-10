#' Use patchwork to create a map with an inset context map or figpatch to stamp
#' an inset image
#'
#' [layer_inset_map] is useful when you want to add an inset to a plot.
#'
#' [make_inset_map] is useful for creating an inset map just using the location
#' with fewer options for customization. In that case, the ... parameters are
#' passed to [layer_context] instead of [patchwork::inset_element]
#'
#' [stamp_inset_img] is useful for applying a logo to a map. The ... parameters
#' are passed to [figpatch::fig]
#'
#' Note, currently, plots created with [layer_inset] do not work with
#' [map_ggsave_ext] using the `single_file = TRUE` parameter.
#'
#' @param inset plot or map created with [ggplot2] passed to p argument of
#'   [patchwork::inset_element]. If both location and context are provided to
#'   [make_inset_map], inset is optional and any provided value is replaced with
#'   a new layer created by [layer_location_context].
#' @param plot,map plot or map created with [ggplot2]
#' @param path image path passed to [figpatch::fig] for [stamp_inset_img]
#' @param img_margin margin around image for [stamp_inset_img] created by
#'   [ggplot2::margin]. Defaults to no margin.
#' @inheritParams layer_location_context
#' @param scale scale of inset map, defaults to 1.
#' @param position inset map position, Default: 'bottomright'. position,
#'   nudge_x, and nudge_y are used to set the left, bottom, top, and right
#'   parameters for [patchwork::inset_element].
#' @param nudge_x,nudge_y nudge x and/or y position of inset map, Default: 0.
#' @inheritParams patchwork::inset_element
#' @inheritDotParams patchwork::inset_element
#' @return ggplot2 map with inset map added using patchwork
#' @rdname layer_inset
#' @export
layer_inset <- function(inset,
                        map = NULL,
                        position = "bottomright",
                        scale = 1,
                        nudge_x = 0,
                        nudge_y = 0,
                        align_to = "full",
                        ...) {
  make_inset_element(
    inset = inset,
    plot = map,
    position = position,
    scale = scale,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    align_to = align_to,
    ...
  )
}

#' @name make_inset_map
#' @rdname layer_inset
#' @inheritParams layer_location_context
#' @export
#' @importFrom ggplot2 ggplot
make_inset_map <-
  function(inset = NULL,
           map = NULL,
           location = NULL,
           context = NULL,
           position = "bottomright",
           scale = 1,
           nudge_x = 0,
           nudge_y = 0,
           align_to = "full",
           ...) {
    if (!is.null(location) && !is.null(context)) {
      if (!is.null(inset)) {
        cli::cli_warn("Replacing the provided inset layer with a new layer
                      from {.fn layer_location_context}")
      }

      inset <-
        ggplot2::ggplot() +
        layer_location_context(
          data = location,
          context = context,
          ...
        )
    }

    make_inset_element(
      inset = inset,
      plot = map,
      position = position,
      scale = scale,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      align_to = align_to
    )
  }

#' @name stamp_inset_img
#' @rdname layer_inset
#' @export
#' @importFrom ggplot2 margin
stamp_inset_img <-
  function(plot,
           path,
           img_margin = ggplot2::margin(0, 0, 0, 0),
           position = "bottomright",
           scale = 1,
           nudge_x = 0,
           nudge_y = 0,
           align_to = "full",
           ...) {
    is_pkg_installed("figpatch")

    inset <- figpatch::fig(path = path, b_margin = img_margin, ...)

    make_inset_element(
      inset = inset,
      plot = plot,
      position = position,
      scale = scale,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      align_to = align_to
    )
  }

#' @noRd
make_inset_element <- function(inset,
                               plot = NULL,
                               position = "bottomright",
                               scale = 1,
                               nudge_x = 0,
                               nudge_y = 0,
                               align_to = "full",
                               ...) {
  is_pkg_installed("patchwork")

  inset_position <-
    get_inset_position(
      scale = scale,
      position = position,
      nudge_x = nudge_x,
      nudge_y = nudge_y
    )

  inset <-
    patchwork::inset_element(
      p = inset,
      left = inset_position$left,
      bottom = inset_position$bottom,
      right = inset_position$right,
      top = inset_position$top,
      align_to = align_to,
      ...
    )

  if (is.null(plot)) {
    return(inset)
  }

  plot +
    inset
}

#' Convert scale, position, nudge_x and nudge_y into the position parameters for
#' patchwork::inset_element
#'
#' @noRd
get_inset_position <- function(scale = 1, position = NULL, nudge_x = 0, nudge_y = 0) {

  # FIXME: This is an incomplete implementation of a scale factor for an inset map
  # top, bottom, left, and right probably should all be based on scale as well
  top <- 0.5
  bottom <- 0.5
  left <- 0.5
  right <- 0.5
  width <- 0.25

  if (is.numeric(scale)) {
    width <- width * scale
  }

  if (grepl("top", position)) {
    top <- 1
    bottom <- bottom + width
  } else if (grepl("bottom", position)) {
    top <- width
    bottom <- 0
  }

  if (grepl("left", position)) {
    left <- 0
    right <- width
  } else if (grepl("right", position)) {
    left <- 1 - width
    right <- 1
  }

  left <- left + nudge_x
  right <- right + nudge_x
  top <- top + nudge_y
  bottom <- bottom + nudge_y

  list(
    "top" = top,
    "right" = right,
    "bottom" = bottom,
    "left" = left
  )
}
