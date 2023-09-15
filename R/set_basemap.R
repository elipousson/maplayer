#' Create a base map by adding the object
#'
#' Add a basemap to a ggplot2 layer.
#'
#' @param x A ggproto object or list of ggproto objects.
#' @param basemap Either a logical vector or ggplot object.
#'
#'   If __logical__ and `TRUE`, add x to [ggplot2::ggplot()]. If `FALSE`, return
#'   x as is.
#'
#'   If a __ggplot__, add x to basemap object.
#'
#'   If a __ggproto__ object (or list that contains a __ggproto__ object), add x
#'   and basemap object to [ggplot2::ggplot()].
#'
#' @inheritParams rlang::args_error_context
#' @export
#' @importFrom rlang is_logical
#' @importFrom ggplot2 ggplot is.ggplot
#' @importFrom cliExtras cli_abort_ifnot
set_basemap <- function(x, basemap = FALSE, call = caller_env()) {
  if (is_true(basemap) && !is.null(x)) {
    # check_gg(x, call = call)
    return(ggplot2::ggplot() + x)
  }

  if (is_false(basemap) || is_null(basemap)) {
    return(x)
  }
  # check_gg(x, call = call)
  # check_ggplot(basemap, call = call)

  combine_gg_list(basemap, x)
}

#' @name make_basemap
#' @rdname set_basemap
#' @export
make_basemap <- function(x, basemap = FALSE, call = caller_env()) {
  set_basemap(x, basemap = basemap, call = call)
}
