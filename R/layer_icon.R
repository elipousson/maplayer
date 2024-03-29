#' Use ggsvg to add a layer with icons at feature locations
#'
#' Use the [ggsvg::geom_point_svg()] function to plot icons using the centroids
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
#'   is ignored. See `map_icons$name` for supported options.
#' @param px Icon size in pixels. See `map_icons$px` for supported options.
#'   Optional but may be necessary to differentiate icons with duplicate names.
#' @param source Icon source. See `map_icons$repo` for supported options.
#'   Optional but may be required to differentiate icons with duplicate names.
#' @param svg Optional. Custom file path or URL with SVG to pass to `svg`
#'   parameter for  [ggsvg::geom_point_svg()].  If `icon` is provided, `svg` is
#'   ignored.
#' @param crs Coordinate reference system; defaults to `NULL`.
#' @inheritDotParams ggsvg::geom_point_svg
#' @example examples/layer_icon.R
#' @seealso
#'  [ggsvg::geom_point_svg()]
#'  [map_icons]
#' @rdname layer_icon
#' @export
#' @importFrom rlang check_installed has_name
#' @importFrom sfext get_sf_col
#' @importFrom ggplot2 aes
layer_icon <- function(data = NULL,
                       iconname_col = "icon",
                       icon = NULL,
                       px = NULL,
                       source = NULL,
                       svg = NULL,
                       crs = getOption("maplayer.crs", default = 3857),
                       ...) {
  rlang::check_installed("rsvg")
  check_dev_installed("ggsvg", "coolbutuseless/ggsvg")

  defaults <- list("geometry" = sfext::get_sf_col(data))

  if (!is.null(iconname_col) && is.null(icon)) {
    data <-
      join_map_icons(
        data = data,
        iconname_col = iconname_col,
        px = px,
        source = source,
        crs = crs
      )

    layer <-
      geom_sf_coordinates(
        mapping = ggplot2::aes(
          svg = .data[["svg"]]
        ),
        data = data,
        geom = ggsvg::geom_point_svg,
        defaults = defaults,
        ...
      )

    return(layer)
  }

  if (!is.null(icon)) {
    svg <-
      get_map_icon(
        icon = icon,
        source = source,
        px = px,
        single = TRUE
      )[["svg"]]
  }

  cli_abort_ifnot(
    "{.arg iconname_col}, {.arg icon},
    or {.arg svg} must be provided." = !is.null(svg)
  )

  geom_sf_coordinates(
    svg = svg,
    data = data,
    geom = ggsvg::geom_point_svg,
    defaults = defaults,
    ...
  )
}

#' @rdname layer_icon
#' @name geom_sf_icon
#' @export
geom_sf_icon <- layer_icon

#' Get subset of icons from map_icons data
#'
#' @param single If `TRUE` and parameters match more than one icon, return an
#'   error. If `FALSE`, return a tibble with all matching icons..
#' @param read If TRUE, read url from `map_icons` to text for SVG using
#'   [readLines()]. Defaults to `TRUE`.
#' @noRd
#' @importFrom dplyr bind_cols
#' @importFrom cliExtras cli_abort_if
get_map_icon <- function(icon = NULL, px = NULL, source = NULL, single = TRUE, read = TRUE) {
  icon_name <- icon
  icon <- map_icons

  if (!is.null(icon_name)) {
    icon <- icon[icon$name %in% icon_name, ]
  }

  if (!is.null(px)) {
    icon <- icon[icon$size %in% as.numeric(px), ]
  }

  if (!is.null(source)) {
    icon <- icon[grepl(source, icon$repo), ]
  }

  cli_abort_if(
    c("{.arg icon} matches {nrow(icon)} icon, not 1.",
      "i" = "Provide the {.arg px} and/or {.arg source} to select a single icon."
    ),
    condition = (nrow(icon) > 1) && single
  )

  if (!read) {
    return(icon)
  }

  dplyr::bind_cols(
    icon,
    "svg" = map_chr(
      icon$url,
      ~ paste(suppressWarnings(readLines(.x)), collapse = "\n")
    )
  )
}

#' Join map icons to a simple feature object with a column with the same name as
#' iconname_col
#'
#' @param fun.geometry sf function to apply before returning data; defaults to
#'   `function(x) sf::st_centroid(sf::st_zm(x))`
#' @noRd
#' @importFrom sf st_centroid st_zm
#' @importFrom sfext check_sf st_transform_ext
#' @importFrom rlang has_name
#' @importFrom dplyr bind_cols left_join
join_map_icons <- function(data = NULL,
                           iconname_col = "icon",
                           source = NULL,
                           px = NULL,
                           crs = NULL,
                           fun.geometry = function(x) sf::st_centroid(sf::st_zm(x)),
                           call = parent.frame()) {
  if (is.null(data)) {
    return(
      function(x) {
        join_map_icons(
          data = x,
          iconname_col = iconname_col,
          px = px,
          source = source,
          crs = crs
        )
      }
    )
  }

  sfext::check_sf(data, call = call)

  cli_abort_ifnot(
    "{.arg iconname_col} {.value {iconname_col}} can't be found
      in {.arg data}." = rlang::has_name(data, iconname_col),
    call = call
  )

  col_icons <- sort(unique(data[[iconname_col]]))

  icons <-
    get_map_icon(
      icon = col_icons,
      source = source,
      px = px,
      single = FALSE
    )

  icons <-
    dplyr::bind_cols(
      icons,
      {{ iconname_col }} := col_icons
    )

  data <-
    dplyr::left_join(
      data,
      icons,
      by = {{ iconname_col }}
    )

  data <- sfext::st_transform_ext(data, crs = crs)

  suppressWarnings(fun.geometry(data))
}
