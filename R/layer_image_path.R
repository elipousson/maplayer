#' Use ggpath to create a layer with images at locations
#'
#' Use the [ggpath::geom_from_path()] function with [sfext::read_sf_exif()] and
#' `stat = "sf_coordinates"` to create a layer showing images at locations.
#'
#' @param data A `sf` object with a column containing file paths that has the
#'   same name as the path_col argument. Optional if "path" is provided for
#'   [sfext::read_sf_exif()]. Required if path is `NULL`. If path is provided,
#'   data is ignored.
#' @param path_col Column name with file paths from data. Defaults to "path"
#'   (path name used by data returned from [sfext::read_sf_exif()])
#' @param segment_params Not implemented: parameters to define segments
#'   connecting images to the location.
#' @param width Width of the image in npc (Normalised Parent Coordinates) passed
#'   to [ggpath::geom_from_path()]; defaults to 0.1.
#' @inheritParams sfext::read_sf_exif
#' @inheritParams ggpath::geom_from_path
#' @inheritParams set_neatline
#' @inheritParams set_basemap
#' @export
#' @importFrom sf st_transform
#' @importFrom sfext read_sf_exif
#' @importFrom rlang has_name
#' @importFrom ggplot2 after_stat
layer_image_path <- function(data = NULL,
                             path = NULL,
                             path_col = "path",
                             width = 0.1,
                             crs = getOption("maplayer.crs", 3857),
                             segment_params = NULL,
                             neatline = FALSE,
                             basemap = FALSE,
                             ...) {
  if (!is.null(path)) {
    data <- sf::st_transform(sfext::read_sf_exif(path), crs = crs)
  }

  check_dev_installed("ggpath", repo = "mrcaseb/ggpath")

  if (!rlang::has_name(data, path_col)) {
    cli_abort("{.arg path_col} {.val {path_col}} can't be found in {.arg data}.")
  }

  path_layer <-
    geom_sf_coordinates(
      geom = ggpath::geom_from_path,
      mapping = aes(
        path = .data[[path_col]],
        x = ggplot2::after_stat(x),
        y = ggplot2::after_stat(y)
      ),
      data = data,
      width = width,
      ...
    )

  # if (!is.null(segment_params)) {
  #   segment_params$label <- segment_params$label %||% ""
  #   segment_mapping <- segment_params$mapping %||% aes()
  #   segment_params$mapping <- NULL
  #   path_layer <-
  #     append(
  #       geom_sf_text_repel(segment_mapping, data = data, !!!segment_mapping),
  #       path_layer
  #     )
  # }

  path_layer <- set_neatline(path_layer, neatline)
  set_basemap(path_layer, basemap = basemap)
}
