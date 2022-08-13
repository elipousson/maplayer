#' Does object inherit class "gg"?
#'
#' @noRd
is_gg <- function(x) {
  inherits(x, "gg") | all(vapply(x, function(x) {inherits(x, "gg")}, FALSE))
}

#' Does object seem to be a neatline layer?
#'
#' @noRd
is_neatline <- function(x) {
  is_gg(x) && (ggplot2::is.Coord(x) | any(vapply(x, ggplot2::is.Coord, FALSE)))
}

