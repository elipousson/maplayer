#' Create a text layer
#'
#' @inheritParams ggplot2::geom_sf_text
#' @export
#' @keywords internal
geom_sf_text_ext <- function(
  mapping = aes(),
  data = NULL,
  ...,
  title_axes = FALSE
) {
  text_layer <- ggplot2::geom_sf_text(
    mapping = mapping,
    data = data,
    ...
  )

  if (!title_axes) {
    return(text_layer)
  }

  list(
    text_layer,
    ggplot2::theme(
      axis.title = ggplot2::element_blank()
    )
  )
}

#' Create a label layer
#'
#' @rdname geom_sf_text_ext
#' @export
geom_sf_label_ext <- function(
  mapping = aes(),
  data = NULL,
  ...,
  family = NULL,
  face = NULL,
  theme = NULL,
  title_axes = FALSE
) {
  theme <- theme %||% ggplot2::theme_get()

  family <- family %||% theme$text$family
  face <- face %||% theme$text$face

  text_layer <- ggplot2::geom_sf_label(
    mapping = mapping,
    data = data,
    family = family,
    fontface = face,
    ...
  )

  if (!title_axes) {
    return(text_layer)
  }

  list(
    text_layer,
    ggplot2::theme(
      axis.title = ggplot2::element_blank()
    )
  )
}
