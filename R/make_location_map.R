#' Create a base map by adding the object
#'
#' Add a basemap to a ggplot2 layer.
#'
#' @param x A ggproto object or list of ggproto objects.
#' @param basemap Either a logical vector or ggplot object.
#'
#'   If __logical__ and `TRUE`, add x to [ggplot2::ggplot()]. If `FALSE`, return x
#'   as is.
#'
#'   If a __ggplot__, add x to basemap object.
#'
#'   If a __ggproto__ object (or list that contains a __ggproto__ object), add x
#'   and basemap object to [ggplot2::ggplot()].
#'
#' @name make_basemap
#' @export
#' @importFrom rlang is_logical
#' @importFrom ggplot2 is.ggproto is.ggplot ggplot
make_basemap <- function(x, basemap = FALSE) {
  if (rlang::is_logical(basemap)) {
    if (basemap) {
      return(ggplot2::ggplot() + x)
    }

    return(x)
  }

  if (ggplot2::is.ggplot(basemap)) {
    return(basemap + x)
  } else if (!is_gg(basemap)) {
    cli_abort(
      "{.arg basemap} must be a {.cls lgl} object or {.cls ggproto} object."
    )
  }

  ggplot2::ggplot() +
    c(basemap, x)
}

#' Make a ggplot map using layer_location_data
#'
#' Location is used as the data parameter of layer_location_data so this
#' function is primarily appropriate for the layer_mapbox (`geom = "mapbox"`).
#'
#' @inheritParams papersize::get_paper
#' @param save If `TRUE`, save file with [ggsave_ext] or [ggsave_social],
#'   requires `basemap = TRUE` and filename is not NULL *or* ... include a name
#'   parameter. Default: `FALSE`
#' @inheritParams sfext::st_bbox_ext
#' @inheritParams layer_location_data
#' @inheritParams ggsave_ext
#' @param ggsave_params List of parameters passed to [ggsave_ext()].
#' @inheritParams mapboxapi::layer_static_mapbox
#' @param bg_layer,fg_layer A ggplot2 layer or a list of ggproto objects (e.g.
#'   scales, labels, etc.) to add to the background or foreground of the primary
#'   map layer defined by `"geom"` and other parameters.
#' @param layer If provided, layer is a ggplot2 layer or a list of ggproto
#'   objects (e.g. scales, labels, etc.) that will be used instead of the
#'   provided location or data.
#' @inheritParams make_basemap
#' @inheritParams set_neatline
#' @param ... Additional parameters passed to [layer_location_data()].
#'
#' @details Using [make_image_map()]:
#'
#' [make_image_map()] wraps [sfext::read_sf_exif()] and [make_location_map()]. It is
#' designed for making simple maps of photos in combination with reference
#' tables.
#'
#' @rdname make_location_map
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom papersize get_paper
make_location_map <- function(location,
                              dist = NULL,
                              diag_ratio = NULL,
                              unit = NULL,
                              asp = NULL,
                              data = NULL,
                              crs = NULL,
                              paper = NULL,
                              width = NULL,
                              height = NULL,
                              units = "in",
                              orientation = NULL,
                              geom = "sf",
                              basemap = TRUE,
                              bg_layer = NULL,
                              layer = NULL,
                              fg_layer = NULL,
                              neatline = FALSE,
                              save = FALSE,
                              ggsave_params = list(dpi = 300, ...),
                              ...) {
  if (!is.null(paper)) {
    paper <-
      papersize::get_paper(
        paper = paper,
        orientation = orientation
      )
  } else if (!is.null(width) | !is.null(height)) {
    paper <-
      papersize::make_page_size(
        width,
        height,
        units,
        asp,
        orientation
      )
  }

  if (is.null(data) && is.null(layer)) {
    data <- location
    location <- NULL
    rlang::check_required(data)
  }

  layer <-
    layer %||% layer_location_data(
      data = data,
      location = location,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp %||% paper$asp,
      crs = crs,
      geom = geom,
      neatline = neatline,
      ...
    )

  stopifnot(
    is.null(bg_layer) || is_gg(bg_layer),
    is.null(fg_layer) || is_gg(fg_layer)
  )

  layer_stack <-
    c(
      bg_layer,
      layer,
      fg_layer
    )

  layer_stack <- make_basemap(layer_stack, basemap)

  layer_stack <- set_neatline(layer_stack, neatline)


  if (!save) {
    return(layer_stack)
  }

  ggsave_params$width <- ggsave_params$width %||% paper$width
  ggsave_params$height <- ggsave_params$height %||% paper$height

  eval_tidy_fn(
    layer_stack,
    params = ggsave_params,
    fn = ggsave_ext
  )

  layer_stack
}

#' @name make_social_map
#' @rdname make_location_map
#' @inheritParams sfext::get_social_image
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom papersize get_social_size
make_social_map <- function(location,
                            dist = NULL,
                            diag_ratio = NULL,
                            unit = NULL,
                            asp = NULL,
                            crs = 3857,
                            image = NULL,
                            platform = NULL,
                            format = NULL,
                            orientation = NULL,
                            basemap = TRUE,
                            geom = "mapbox",
                            save = FALSE,
                            ggsave_params = list(filetype = "jpeg", dpi = 72, ...),
                            ...) {
  image_size <-
    papersize::get_social_size(
      name = image,
      platform = platform,
      format = format,
      orientation = orientation
    )

  asp <- asp %||% image_size$asp

  bbox <-
    st_bbox_ext(
      x = location,
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp,
      crs = crs
    )

  map_layer <-
    layer_location_data(
      data = bbox,
      geom = geom,
      ...
    )

  map_layer <- make_basemap(map_layer, basemap)

  if (save) {
    ggsave_params$width <- ggsave_params$width %||% image_size$width
    ggsave_params$height <- ggsave_params$height %||% image_size$height

    eval_tidy_fn(
      map_layer,
      params = ggsave_params,
      fn = ggsave_social
    )
  }

  map_layer
}

#' @name make_image_map
#' @rdname make_location_map
#' @param image_path path to location of images for [make_image_map()]
#' @param image_geom For [make_image_map()], geom to use with layer_markers to
#'   mark the location of images (based on EXIF metadata).
#' @inheritParams layer_markers
#' @export
#' @importFrom sfext st_bbox_ext
#' @importFrom filenamr read_exif
make_image_map <- function(image_path,
                           location = NULL,
                           dist = NULL,
                           diag_ratio = NULL,
                           unit = NULL,
                           asp = NULL,
                           data = NULL,
                           crs = 3857,
                           paper = "Letter",
                           orientation = NULL,
                           geom = "mapbox",
                           style_url = NULL,
                           basemap = TRUE,
                           bg_layer = NULL,
                           fg_layer = NULL,
                           save = FALSE,
                           ggsave_params = list(dpi = 300, ...),
                           image_geom = "label",
                           groupname_col = NULL,
                           group_meta = NULL,
                           number = FALSE,
                           num_by_group = FALSE,
                           num_style = NULL,
                           num_start = 1,
                           suffix = NULL,
                           sort = "dist_xmin_ymax",
                           desc = FALSE,
                           ...) {
  images <-
    filenamr::read_exif(
      path = image_path,
      geometry = TRUE
    )

  location <-
    location %||%
    sfext::st_bbox_ext(
      sf::st_union(images),
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp,
      crs = crs
    )

  make_location_map(
    location = location,
    geom = geom,
    bg_layer = bg_layer,
    layer = layer_markers(
      data = images,
      geom = image_geom,
      crs = crs,
      groupname_col = groupname_col,
      group_meta = group_meta,
      number = number,
      num_by_group = num_by_group,
      num_style = num_style,
      num_start = num_start,
      suffix = suffix,
      sort = sort,
      desc = desc,
      ...
    ),
    fg_layer = fg_layer,
    save = save,
    ggsave_params = ggsave_params,
    style_url = style_url
  )
}
