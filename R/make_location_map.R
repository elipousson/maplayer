#' Make a ggplot map using `layer_location_data()`
#'
#' Location is used as the data parameter of layer_location_data so this
#' function is primarily appropriate for the layer_mapbox (`geom = "mapbox"`).
#'
#' @param location A sf object passed to [layer_location_data()] with data if
#'   layer is `NULL`. location is ignored if layer is provided. If data is
#'   `NULL`, location is passed as data to facilitate using this function with
#'   `geom = "mapbox"` where data is used to define the map area. Defaults to
#'   `NULL`
#' @inheritParams papersize::get_paper
#' @inheritParams sfext::st_bbox_ext
#' @inheritParams layer_location_data
#' @inheritParams set_neatline
#' @inheritParams papersize::ggsave_ext
#' @param ... Additional parameters passed to [layer_location_data()] for
#'   [make_location_map()] or [make_social_map()] or to [layer_markers()] for
#'   [make_image_map()].
#' @example examples/make_location_map.R
#' @details Using [make_image_map()]:
#'
#' [make_image_map()] wraps [sfext::read_sf_exif()] and [make_location_map()].
#' It is designed for making simple maps of photos in combination with reference
#' tables.
#'
#' @rdname make_location_map
#' @export
#' @importFrom papersize get_paper make_page_size
#' @importFrom rlang check_required
make_location_map <- function(location = NULL,
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
                              addon = NULL,
                              neatline = FALSE,
                              labs_ext_params = list(...),
                              save = FALSE,
                              ggsave_params = list(dpi = 300, ...),
                              ...,
                              env = caller_env(),
                              call = caller_env()) {
  if (!is.null(paper)) {
    paper <- papersize::get_paper(
      paper = paper,
      orientation = orientation
    )
  } else if (!is.null(width) || !is.null(height)) {
    paper <- papersize::make_page_size(
      width,
      height,
      units,
      asp,
      orientation
    )
  }

  if (save) {
    ggsave_params$width <- ggsave_params$width %||% paper$width
    ggsave_params$height <- ggsave_params$height %||% paper$height
  }

  if (is.null(data) && is.null(layer)) {
    check_required(location)
    data <- location
    location <- NULL
  }

  layer <- layer %||% layer_location_data(
    data = data,
    location = location,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    asp = asp %||% paper$asp,
    crs = crs,
    geom = geom,
    ...
  )

  make_layer_map(
    basemap = basemap,
    bg_layer = bg_layer,
    layer = layer,
    fg_layer = fg_layer,
    addon = addon,
    neatline = neatline,
    labs_ext_params = labs_ext_params,
    save = save,
    ggsave_params = ggsave_params,
    env = env,
    call = call
  )
}

#' @name make_social_map
#' @rdname make_location_map
#' @inheritParams papersize::get_social_size
#' @export
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
                            ggsave_params = list(fileext = "jpeg", dpi = 72, ...),
                            ...) {
  image_size <- papersize::get_social_size(
    name = image,
    platform = platform,
    format = format,
    orientation = orientation
  )

  bbox <- st_bbox_ext(
    x = location,
    dist = dist,
    diag_ratio = diag_ratio,
    unit = unit,
    asp = asp %||% image_size$asp,
    crs = crs
  )

  map_layer <- make_layer_map(
    layer = layer_location_data(
      data = bbox,
      geom = geom,
      ...
    ),
    basemap = basemap,
    neatline = NULL,
    save = FALSE
  )

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
#' @importFrom sf st_union
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
  rlang::check_installed("filenamr")
  images <-
    filenamr::read_exif(
      path = image_path,
      geometry = TRUE
    )

  location <- location %||%
    sfext::st_bbox_ext(
      sf::st_union(images),
      dist = dist,
      diag_ratio = diag_ratio,
      unit = unit,
      asp = asp,
      crs = crs
    )

  marker_layer <- layer_markers(
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
  )

  if (is_bare_list(fg_layer)) {
    fg_layer <- c(
      marker_layer,
      fg_layer
    )
  } else {
    fg_layer <- list(
      marker_layer,
      fg_layer
    )
  }

  make_location_map(
    location = location,
    geom = geom,
    bg_layer = bg_layer,
    fg_layer = fg_layer,
    save = save,
    ggsave_params = ggsave_params,
    style_url = style_url
  )
}

#' @name make_layer_map
#' @rdname make_location_map
#' @param labs_ext_params Optional parameters passed to [labs_ext()].
#' @param ggsave_params List of parameters passed to [papersize::ggsave_ext()].
#' @inheritParams mapboxapi::layer_static_mapbox
#' @param layer A ggplot2 layer or a list of ggproto objects. If layer is
#'   provided, all parameters passed to [layer_location_data()] (including data,
#'   location, dist, diag_ratio, unit, asp, crs, and geom) will be ignored. In
#'   this case, the function simply stacks the bg_layer, layer, and fg_layer
#'   objects then applies the basemap and neatline (using the [set_basemap()]
#'   and [set_neatline()] helper functions.)
#' @param bg_layer,fg_layer,addon A ggplot2 layer or a list of ggproto objects
#'   (e.g. scales, labels, etc.) to add to the background or foreground of the
#'   primary map layer defined by `"geom"` and other parameters. If the geom
#'   creates an opaque layer or layer is an opaque layer (e.g. a layer produced
#'   by [layer_mapbox()]) that covers the full map extent, the bg_layer will not
#'   be visible.
#' @param save If `TRUE`, save file with [ggsave_ext] using `ggsave_params`.
#'   Defaults to `FALSE`.
#' @param env Environment for evaluation of [labs_ext()] if labs_ext_params is
#'   supplied.
#' @inheritParams set_basemap
#' @inheritParams set_neatline
#' @export
make_layer_map <- function(bg_layer = NULL,
                           layer = NULL,
                           fg_layer = NULL,
                           addon = NULL,
                           basemap = NULL,
                           neatline = NULL,
                           labs_ext_params = NULL,
                           save = FALSE,
                           ggsave_params = list(width = 5, height = 4, unit = "in", dpi = 300),
                           env = caller_env(),
                           call = caller_env()) {
  # FIXME: check_gg is too sensitive but there should still be some input check
  # check_gg(layer, allow_null = TRUE)
  # check_gg(bg_layer, allow_null = TRUE)
  # check_gg(fg_layer, allow_null = TRUE)

  layer_stack <- set_basemap(bg_layer, basemap = basemap, call = call)

  layer_stack <- combine_gg_list(layer_stack, layer)

  layer_stack <- combine_gg_list(layer_stack, fg_layer)

  layer_stack <- combine_gg_list(layer_stack, addon)

  if (!is_empty(labs_ext_params)) {
    layer_stack <- layer_stack +
      eval_tidy_fn(
        params = labs_ext_params,
        fn = labs_ext,
        env = env,
        call = call
      )
  }

  layer_stack <- set_neatline(layer_stack, neatline)

  if (save) {
    eval_tidy_fn(
      layer_stack,
      params = ggsave_params,
      fn = ggsave_ext,
      call = call
    )
  }

  layer_stack
}
