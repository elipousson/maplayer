#' Layer location data into a ggplot2 map
#'
#' Helper function to make a ggplot2 layer from data returned by
#' [get_location_data()]. For text geoms, the required aesthetic mapping is
#' set based on the name_col but values passed to mapping take precedence.
#'
#' @section Using the geom parameter:
#'
#' This function provides a convenient option for filtering data by location
#' while calling a different layer function from ggplot2, maplayer, or a
#' different package.
#'
#' Options for the geom parameter from [ggplot2]:
#'
#' - "sf" ([ggplot2::geom_sf] - default)
#' - "sf_text" or "text ([ggplot2::geom_sf_text])
#' - "sf_label" or "label ([ggplot2::geom_sf_label])
#'
#' Options for the geom parameter included in this package include:
#'
#' - "location" ([layer_location])
#' - "context" or "location_context" ([layer_location_context])
#' - "icon" ([layer_icon]),
#' - "mapbox" ([layer_mapbox])
#' - "markers" ([layer_markers])
#' - "numbers" or "numbered" ([layer_numbers])
#' - "mark" or "marked" ([layer_mark])
#'
#' Options for the geom parameter from other suggested packages include:
#'
#'   - "textsf" ([geomtextpath::geom_textsf])
#'   - "labelsf" ([geomtextpath::geom_labelsf])
#'   - "text_repel" ([ggrepel::geom_text_repel])
#'   - "label_repel" ([ggrepel::geom_label_repel])
#'   - "sf_pattern" or "pattern" ([ggpattern::geom_sf_pattern])
#'
#' Both stat = "sf_coordinates" is automatically added to the parameters for
#' both [ggrepel] functions.
#'
#' @section Using the layer_fn parameter:
#'
#' layer_fn can be a purrr style lamba function (converted with [rlang::as_function]) or a function.
#' If layer_fn is a function, the mapping parameter is used. If layer_fn is a lambda function
#'
#'
#' @param geom A character string indicating which ggplot2 geom to use, Default:
#'   'sf'. Options include "sf" ([ggplot2::geom_sf]), "icon" ([layer_icon]),
#'   "markers" ([layer_markers]), "sf_text" ([ggplot2::geom_sf_text]), and "sf_label"
#'   ([ggplot2::geom_sf_label]). See details for a full list.
#' @param layer_fn ggplot2 geom or custom function using lambda syntax. Use for
#'   passing custom mapping functions to layer_location_data beyond the
#'   supported geom options.
#' @param unit unit to adjust location by dist or diag_ratio; defaults to
#'   "meter"
#' @param label_col Column name or id for a column with the text or labels to
#'   pass to any text geom.
#' @param ... Parameters passed to selected geom
#' @inheritParams getdata::get_location_data
#' @inheritParams ggplot2::geom_sf
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
           layer_fn = NULL,
           crop = TRUE,
           trim = FALSE,
           from_crs = getOption("maplayer.from_crs", 4326),
           crs = getOption("maplayer.crs", 3857),
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
        class = "sf"
      )

    if (!is.null(layer_fn)) {
      if (!is_function(layer_fn)) {
        return(use_fn(data, layer_fn))
      }

      return(layer_fn(data, mapping = mapping, ...))
    }

    params <- list2(...)

    # These geom functions also take a geom parameter
    # The geom parameter is set to the default value
    maplayer_geoms_take_geom <- c("markers", "numbers", "numbered")
    maplayer_geoms <- c("icon", "mapbox", "mark", "marked", "location", "context", "location_context", maplayer_geoms_take_geom)

    # These text geom functions require a name aesthetic parameter and/or a stat parameter
    ggplot_text_geoms <- c("text", "sf_text", "label", "sf_label")
    ggrepel_geoms <- c("text_repel", "label_repel")
    geomtextpath_geoms <- c("textsf", "labelsf")

    text_geoms <- c(ggplot_text_geoms, geomtextpath_geoms, ggrepel_geoms)

    ggpattern_geoms <- c("pattern", "sf_pattern")

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
        "text" = ggplot2::geom_sf_text,
        "sf_text" = ggplot2::geom_sf_text,
        "label" = ggplot2::geom_sf_label,
        "sf_label" = ggplot2::geom_sf_label,
        "icon" = layer_icon,
        "mapbox" = layer_mapbox,
        "markers" = layer_markers,
        "numbers" = layer_numbers,
        "numbered" = layer_numbers,
        "mark" = layer_marked,
        "marked" = layer_marked,
        "location" = layer_location,
        "context" = layer_location_context,
        "location_context" = layer_location_context,
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

    # Reset geom to default for layer_markers or layer_numbers
    if (geom_chr %in% maplayer_geoms_take_geom) {
      params$geom <-
        switch(geom_chr,
          "markers" = "sf",
          "numbers" = "label",
          "numbered" = "label"
        )
    }

    # Reset defaults for geom_sf_text and geom_sf_label (and functions based on those)
    if (any(has_name(init_params, c("nudge_x", "nudge_y")))) {
      params$position <- NULL
    } else {
      params$nudge_x <- NULL
      params$nudge_y <- NULL
    }

    # Reset default for ggrepel functions
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
