
#' Create a ggplot2 layer with map markers or numbered markers
#'
#' If make is `TRUE`, groupname_col, group_meta, crs, and fn is all passed on to
#' [make_markers].
#'
#' @inheritParams make_markers
#' @param make If `TRUE`, pass data to [make_markers].
#' @param number If `TRUE`, number markers using [layer_markers()] (not
#'   currently supported)
#' @inheritParams sfext::number_features
#' @param style Marker style; defaults to `NULL` for [layer_markers()] (supports
#'   "facet"); defaults to "roundrect" for [layer_markers()] when numbered =
#'   `TRUE` (default is only supported option at present).
#' @inheritParams  layer_location_data
#' @param ... Additional parameters passed to [layer_group_data()]
#' @return ggplot2 layers
#' @example examples/layer_markers.R
#' @name layer_markers
#' @md
#' @export
layer_markers <- function(data,
                          mapping = NULL,
                          geom = "sf",
                          make = FALSE,
                          groupname_col = NULL,
                          group_meta = NULL,
                          crs = getOption("maplayer.crs", default = 3857),
                          number = FALSE,
                          num_by_group = FALSE,
                          num_style = NULL,
                          num_start = 1,
                          suffix = NULL,
                          sort = "dist_xmin_ymax",
                          desc = FALSE,
                          fn = NULL,
                          ...) {
  if (make) {
    data <-
      make_markers(
        data = data,
        groupname_col = groupname_col,
        group_meta = group_meta,
        crs = crs,
        fn = fn
      )
  }

  if (number) {
    col <- NULL
    if (num_by_group) {
      col <- groupname_col
    }

    data <-
      sfext::number_features(
        x = data,
        col = col,
        sort = sort,
        desc = desc,
        num_style = num_style,
        num_start = num_start,
        suffix = suffix
      )

    mapping <-
      modify_mapping(
        mapping = mapping,
        label = "number"
      )
  }

  if (!is.null(groupname_col)) {
    if (sfext::is_point(data) || sfext::is_multipoint(data)) {
      mapping <-
        modify_mapping(
          mapping = mapping,
          color = groupname_col
        )
    } else {
      mapping <-
        modify_mapping(
          mapping = mapping,
          fill = groupname_col
        )
    }
  }

  layer_location_data(
    data = data,
    geom = geom,
    mapping = mapping,
    ...
  )
}

#' @rdname layer_markers
#' @name layer_numbers
#' @param size Marker size, Default: 5
#' @param num_by_group If `TRUE`, numbers are added by group based on
#'   groupname_col.
#' @inheritParams sfext::number_features
#' @inheritParams ggplot2::geom_sf_label
#' @param hjust,vjust Horizontal and vertical justification.
#' @param ... Additional parameters passed to [layer_location_data()]
#' @export
#' @importFrom ggplot2 aes unit
#' @importFrom utils modifyList
#' @importFrom dplyr arrange mutate row_number
layer_numbers <- function(data,
                          mapping = NULL,
                          geom = "label",
                          make = FALSE,
                          groupname_col = NULL,
                          style = "roundrect",
                          size = 5,
                          sort = "dist_xmin_ymax",
                          num_by_group = FALSE,
                          num_style = NULL,
                          num_start = 1,
                          suffix = NULL,
                          desc = FALSE,
                          fn = NULL,
                          crs = getOption("maplayer.crs", default = 3857),
                          label.size = 0.0,
                          label.padding = ggplot2::unit(size / 10, "lines"),
                          label.r = label.padding * 1.5,
                          hjust = 0.5,
                          vjust = 0.5,
                          ...) {
  if ("roundrect" %in% style) {
    label.size <- 0.0
    label.padding <- ggplot2::unit(size / 10, "lines")
    label.r <- label.padding * 1.5
    hjust <- 0.5
    vjust <- 0.5
  }

  layer_markers(
    data = data,
    mapping = mapping,
    number = TRUE,
    geom = geom,
    sort = sort,
    num_style = num_style,
    suffix = suffix,
    num_by_group = num_by_group,
    size = size,
    label.size = label.size,
    label.padding = label.padding,
    label.r = label.r,
    hjust = hjust,
    vjust = vjust,
    ...
  )
}


#' Make map markers from a simple feature object

#' @name make_markers
#' @rdname layer_markers
#' @param join Spatial relation function to combine data with group_meta, passed
#'   to [sf::st_join()]. Defaults to [sf::st_intersects()].
#' @param groupname_col Group column name, used to join group metadata if
#'   group_meta is a non-spatial data frame; Default: `NULL`
#' @param group_meta Group metadata as a data frame or sf object that intersect
#'   with markers (using join function); Default: `NULL`
#' @param crs Coordinate reference system for markers, Default: `NULL`
#' @param fn Function to apply to data before results; gives warning if data is
#'   grouped; Default: `NULL`
#' @param geo If `FALSE`, pass data to [getdata::get_location_data()] with `geo
#'   = TRUE` parameter.
#' @inheritParams sfext::df_to_sf
#' @param point If `TRUE` and data does not have POINT or MULTIPOINT geometry,
#'   convert to POINT data using [sf::st_centroid()].
#' @param ... Additional parameters passed to [get_location_data()] when using
#'   `make = TRUE` to pass data to [make_markers]
#' @export
#' @importFrom sf st_intersects st_join st_centroid
#' @importFrom getdata get_location_data
#' @importFrom sfext df_to_sf is_geom_type
#' @importFrom dplyr left_join filter group_by
make_markers <- function(data,
                         groupname_col = NULL,
                         group_meta = NULL,
                         join = sf::st_intersects,
                         geo = FALSE,
                         coords = c("lon", "lat"),
                         address = "address",
                         point = TRUE,
                         crs = NULL,
                         fn = NULL,
                         ...) {
  if (!geo) {
    data <-
      getdata::get_location_data(
        data = data,
        crs = crs,
        geo = TRUE,
        ...
      )
  } else if (is.data.frame(data) && has_name(data, address)) {
    data <- sfext::df_to_sf(data, address = address, coords = coords, crs = crs)
  }

  if (!is.null(group_meta)) {
    if (is.data.frame(group_meta) && !is.null(groupname_col)) {
      data <-
        dplyr::left_join(data, group_meta, by = groupname_col)
    } else if (is_sf(group_meta) && is_sf(data)) {
      data <- sf::st_join(x = data, y = group_meta, join = join)
    }
  }

  if (!is.null(groupname_col)) {
    data <- dplyr::filter(data, !is.na(.data[[groupname_col]]))

    data <- dplyr::group_by(data, .data[[groupname_col]])
  }

  # Convert to POINT if any other geometry
  if (!all(sfext::is_geom_type(data, type = c("POINT", "MULTIPOINT"))) && point) {
    data <- suppressWarnings(sf::st_centroid(data))
  }

  if (!is.null(groupname_col) && !is.null(fn)) {
    cli_warn("Function passed to {.arg fn} is being applied to grouped data.")
  }

  use_fn(data, fn)
}
