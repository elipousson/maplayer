#' Layer location data into a ggplot2 map
#'
#' Helper function to make a ggplot2 layer from data returned by
#' [get_location_data()]. For text geoms, the required aesthetic mapping is
#' set based on the name_col but values passed to mapping take precedence.
#'
#' @details Supported geom function options:
#'
#' Options for the geom parameter from the overedge package include:
#'
#' - "icon" ([layer_icon]),
#' - "mapbox" ([layer_mapbox]),
#' - "markers" ([layer_markers]),
#' - "numbers" ([layer_numbers])
#'
#' Options for the geom parameter from other packages include:
#'
#'   - "textsf" ([geomtextpath::geom_textsf])
#'   - "labelsf" ([geomtextpath::geom_labelsf])
#'   - "text_repel" ([ggrepel::geom_text_repel])
#'   - "label_repel" ([ggrepel::geom_label_repel])
#'   - "mark" ([birdseyeview::layer_show_mark])
#'   - "location" ([birdseyeview::layer_show_location])
#'   - "context" ([birdseyeview::layer_show_context])
#'   - "pattern" ([ggpattern::geom_sf_pattern])
#'
#' Alternatively, use the "geom_fn" parameter to pass a function that returns a
#' ggplot2 layer to use instead of one of the preset geom functions.
#'
#' @param geom A character string indicating which ggplot2 geom to use, Default:
#'   'sf'. Options include "sf" ([ggplot2::geom_sf]), "icon" ([layer_icon]),
#'   "markers" ([layer_markers]), "text" ([ggplot2::geom_sf_text]), and "label"
#'   ([ggplot2::geom_sf_label]). See details for a full list.
#' @param geom_fn ggplot2 geom or custom function using lambda syntax. Use for
#'   passing custom mapping functions to layer_location_data beyond the
#'   supported geom options.
#' @param unit unit to adjust location by dist or diag_ratio; defaults to
#'   "meter"
#' @param label_col Column name or id for a column with the text or labels to
#'   pass to any text geom.
#' @param ... Parameters passed to selected geom
#' @inheritParams getdata::get_location_data
#' @inheritParams ggplot2::geom_sf
#' @return ggplot2 geom
#' @rdname layer_location_data
#' @family layer
#' @export
#' @importFrom ggplot2 geom_sf geom_sf_text geom_sf_label
#' @importFrom purrr discard
#' @importFrom utils modifyList
layer_location_data <-
  function(mapping = NULL,
           data = NULL,
           geom = "sf",
           location = NULL,
           dist = getOption("maplayer.dist"),
           diag_ratio = getOption("maplayer.diag_ratio"),
           unit = getOption("maplayer.unit", default = "meter"),
           asp = getOption("maplayer.asp"),
           package = getOption("maplayer.data_package"),
           filetype = getOption("maplayer.data_filetype"),
           fn = NULL,
           geom_fn = NULL,
           crop = TRUE,
           trim = FALSE,
           from_crs = getOption("maplayer.from_crs"),
           crs = getOption("maplayer.crs"),
           label_col = "name",
           ...) {
    data <-
      getdata::get_location_data(
        location = location,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp,
        data = data,
        package = package,
        filetype = filetype,
        fn = fn,
        crop = crop,
        trim = trim,
        from_crs = from_crs,
        crs = crs,
        class = "sf",
        ...
      )

    if (!is.null(geom_fn)) {
      if(!is_function(geom_fn)) {
        return(use_fn(data, geom_fn, ...))
      }

      return(geom_fn(data, mapping = mapping, ...))
    }

    params <- list2(...)

    maplayer_geoms <- c("icon", "mapbox", "markers", "numbers", "mark", "location", "context")
    ggpattern_geoms <- c("pattern", "sf_pattern")

    ggrepel_geoms <- c("text_repel", "label_repel")
    geomtextpath_geoms <- c("textsf", "labelsf")
    text_geoms <- c("text", "label", geomtextpath_geoms, ggrepel_geoms)

    # Match geoms
    geom <-
      arg_match(
        geom,
        c("sf", maplayer_geoms, text_geoms, ggpattern_geoms)
      )

    is_geom_pkg_installed(geom)

    # Assign aesthetics for text/label geoms
    if (geom %in% text_geoms) {
      mapping <- modify_mapping(mapping = mapping, label = label_col)

      if (geom %in% ggrepel_geoms) {
        mapping <- modify_mapping(mapping = mapping, data = data)

        params <- c(params, stat = "sf_coordinates")
      }
    }

    geom_chr <- geom

    geom <-
      switch(geom_chr,
        "sf" = ggplot2::geom_sf,
        "sf_text" = ggplot2::geom_sf_text,
        "sf_label" = ggplot2::geom_sf_label,
        "text" = ggplot2::geom_sf_text,
        "label" = ggplot2::geom_sf_label,
        "icon" = layer_icon,
        "mapbox" = layer_mapbox,
        "markers" = layer_markers,
        "numbers" = layer_numbers,
        "mark" = layer_marked,
        "location" = layer_location,
        "context" = layer_location_context,
        "text_repel" = ggrepel::geom_text_repel,
        "label_repel" = ggrepel::geom_label_repel,
        "textsf" = geomtextpath::geom_textsf,
        "labelsf" = geomtextpath::geom_labelsf,
        "pattern" = ggpattern::geom_sf_pattern,
        "sf_pattern" = ggpattern::geom_sf_pattern
      )

    init_params <- params

    params <-
      modify_fn_fmls(
        params = params,
        fn = geom,
        mapping = mapping,
        data = data,
        keep.null = TRUE
      )

    # FIXME: This does not seem like the best way of dealing with the default params issue
    if (any(has_name(init_params, c("nudge_x", "nudge_y")))) {
      params$position <- NULL
    } else {
      params$nudge_x <- NULL
      params$nudge_y <- NULL
    }

    if (geom_chr %in% c("markers", "numbers")) {
      params$geom <- geom_chr
    }

    if (!has_name(init_params, c("direction")) && (geom_chr %in% ggrepel_geoms)) {
      params$direction <- "both"
    }

    exec(geom, !!!params)
  }

#' Modify function parameters
#'
#' @noRd
#' @importFrom purrr discard
#' @importFrom utils modifyList
modify_fn_fmls <- function(params, fn, keep_missing = FALSE, keep.null = FALSE, ...) {
  fmls <- fn_fmls(fn)

  if (!keep_missing) {
    fmls <- purrr::discard(fmls, is_missing)
  }

  params <- c(list2(...), params)

  utils::modifyList(
    fmls,
    params,
    keep.null = keep.null
  )
}
