#' Add a SVG icons to a ggplot2 plot based on a simple feature object
#'
#' Use the  [ggsvg::geom_point_svg()] function to plot icons using the centroids
#' from the input simple feature object to set the icon location.
#'
#' @param data A `sf` object. Any objects with polygon geometry are converted to
#'   points using [sf::st_centroid()].
#' @param iconname_col The column name in the input data to use as the icon
#'   name. If the name matches multiple icons, the first match from `map_icons`
#'   is used. You may provide a px or source value to select a different match
#'   if needed but, in that case, all icons must use the same px or source
#'   value. Note that the icon column should not be mapped with
#'   [ggplot2::aes()].
#' @param icon Icon name. Default `NULL`. If `icon` is provided, `iconname_col`
#'   is not used.
#' @param px Icon size in pixels. See `map_icons$px` for supported options.
#'   Optional but may be necessary to differentiate icons with duplicate names.
#' @param source Icon source. See `map_icons$repo` for supported options.
#'   Optional but may be required to differentiate icons with duplicate names.
#' @param svg Optional. Custom file path or URL with SVG to pass to `svg`
#'   parameter for  [ggsvg::geom_point_svg()].  If `icon` is provided, `svg` is
#'   not used.
#' @param color SVG color passed to  [ggsvg::geom_point_svg()]. default color is
#'   set to "black".
#' @param crs Coordinate reference system; defaults to `NULL`.
#' @param ... Additional parameters to  [ggsvg::geom_point_svg()].
#' @example examples/layer_icon.R
#' @seealso
#'  [ggsvg::geom_point_svg()]
#'  [map_icons]
#' @rdname layer_icon
#' @export
#' @importFrom sf st_geometry_type st_centroid st_coordinates
#' @importFrom dplyr left_join rename bind_cols filter
#' @importFrom sfext check_sf st_transform_ext
#' @importFrom ggplot2 aes
#' @importFrom stringr str_detect
layer_icon <- function(data = NULL,
                       iconname_col = "icon",
                       icon = NULL,
                       px = NULL,
                       source = NULL,
                       svg = NULL,
                       color = "black",
                       crs = getOption("maplayer.crs", default = 3857),
                       ...) {
  is_pkg_installed(pkg = "ggsvg", repo = "coolbutuseless/ggsvg")

  if (is_sf(data) && !sfext::is_point(data)) {
    data <- suppressWarnings(sf::st_centroid(data))
  }

  sfext::check_sf(data)

  data <- sfext::st_transform_ext(data, crs = crs)

  if ((iconname_col %in% names(data)) & is.null(icon)) {
    icon_options <-
      dplyr::rename(
        map_icons,
        svg_url = url,
        {{ iconname_col }} := name
      )

    if (!is.null(px)) {
      icon_options <-
        dplyr::filter(
          icon_options,
          .data$px == px
        )
    }

    if (!is.null(source)) {
      icon_options <-
        dplyr::filter(
          icon_options,
          .data$repo == source
        )
    }

    data <-
      dplyr::left_join(
        data,
        icon_options,
        by = {{ iconname_col }}
      )

    data <-
      dplyr::mutate(
        dplyr::rowwise(data),
        svg_url = paste(readLines(svg_url), collapse = "\n")
      )

    icon_layer <-
      ggsvg::geom_point_svg(
        data = data,
        ggplot2::aes(
          geometry = .data[[attributes(data)$sf_column]],
          svg = .data[["svg_url"]]
        ),
        stat = "sf_coordinates",
        ...
      )
  } else if (!is.null(icon)) {
    icon <- dplyr::filter(map_icons, .data$name == icon)

    if (!is.null(px)) {
      icon <- dplyr::filter(icon, .data$size == px)
    }

    if (!is.null(source)) {
      icon <- dplyr::filter(icon, stringr::str_detect(repo, source))
    }

    if (nrow(icon) > 1) {
      cli_abort(
        c("The provided parameters match more than one icon.",
          "i" = "Provide the `px` and/or `source` to select a single icon."
        )
      )
    }

    icon_layer <-
      suppressWarnings(
        ggsvg::geom_point_svg(
          data = data,
          ggplot2::aes(geometry = .data[[attributes(data)$sf_column]]),
          svg = paste(readLines(icon$url), collapse = "\n"),
          stat = "sf_coordinates",
          color = color,
          defaults = list(color = "black"),
          ...
        )
      )
  } else if (!is.null(svg)) {
    icon_layer <-
      suppressWarnings(
        ggsvg::geom_point_svg(
          data = data,
          ggplot2::aes(x = lon, y = lat),
          svg = svg,
          color = color,
          ...
        )
      )
  }

  icon_layer
}

#' @rdname layer_icon
#' @name geom_sf_icon
#' @export
geom_sf_icon <- layer_icon
