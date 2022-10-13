#' Create a layer with an arrow or segment from and to specified locations
#'
#' A wrapper for [ggplot2::geom_segment], [ggplot2::geom_curve()],
#' [ggarchery::geom_arrowsegment()], [ggforce::geom_diagonal0()],
#' [ggforce::geom_link()] that makes it easier to specify the start and end of
#' the segment using any object supported by the [sfext::as_xy()] function.
#'
#' @param mapping aesthetic mapping overwritten with x, y, xend, and yend values
#'   based on provided from and to parameters.
#' @param data Required if from or to are character vectors to [sfext::as_xy()]
#' @param from,to Required. Passed to x parameter of [sfext::as_xy()] (using `nm
#'   = c("xend", "yend")`) for the to parameter.
#' @inheritParams sfext::as_xy
#' @param geom Character string for geom to use  c("segment", "curve",
#'   "arrowsegment", "diagonal0", "link") or a geom function.
#' @seealso
#'  [ggplot2::reexports()], [ggplot2::geom_segment()], [ggplot2::aes()]
#' @name layer_arrow
#' @export
#' @importFrom ggplot2 arrow geom_curve aes
#' @importFrom rlang arg_match
#' @importFrom sfext as_xy
#' @importFrom dplyr bind_cols
#' @importFrom utils modifyList
layer_arrow <- function(mapping = NULL,
                        data = NULL,
                        crs = NULL,
                        from,
                        to,
                        geom = "segment",
                        ...) {

  if (!is_fn(geom)) {
    geom <-
      rlang::arg_match(
        geom,
        c("segment", "curve", "arrowsegment", "diagonal0", "link")
      )

    if (geom == "arrowsegment") {
      is_pkg_installed("ggarchery", "mdhall272/ggarchery")
    } else if (geom %in% c("diagonal0", "geom_link")) {
      is_pkg_installed("ggforce")
    }

    geom <-
      switch(geom,
             "segment" = ggplot2::geom_segment,
             "curve" = ggplot2::geom_curve,
             "arrowsegment" = ggarchery::geom_arrowsegment,
             "diagonal0" = ggforce::geom_diagonal0,
             "link" = ggforce::geom_link
      )
  }

  data <-
    dplyr::bind_cols(
      sfext::as_xy(from, data = data, crs = crs),
      sfext::as_xy(to, data = data, nm = c("xend", "yend"), crs = crs)
    )

  mapping <-
    utils::modifyList(
      aes(
        x = .data[["x"]], y = .data[["y"]],
        xend = .data[["xend"]], yend = .data[["yend"]]
      ),
      mapping %||% aes()
    )

  geom(
    mapping = mapping,
    data = data,
    ...
  )
}
