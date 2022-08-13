#' Label simple feature objects in a location
#'
#' Label markers, streets, areas, or other simple feature objects using any of
#' the following geoms: "text", "sf_text", "label", "sf_label", "textsf",
#' "labelsf", "text_repel", or "label_repel".
#'
#' Note: Unlike in some getdata package functions, fn is applied to all of the
#' data; not a subset of the data based on location. For this function, dist and
#' unit are both used by [sfext::st_clip()] (not by [layer_location_data()])
#'
#' The function also overrides the label aesthetics to hide the colored letters
#' that would otherwise appear when using a theme legend.
#'
#' @param data Data to use for labels (must be an sf object or a data frame that
#'   can be converted to an sf object using [sfext::as_sf()])
#' @param location Location to label (if not specified the data is assumed to
#'   conver the whole location);
#' @param fn Function to apply to data before creating labels; can be used in
#'   the creation of the label_col
#' @param label_col Label column name
#' @param geom A geom to use "text", "label", "textsf", "labelsf", "text_repel",
#'   or "label_repel"
#' @param mapping Aesthetic mapping, Default: `NULL`
#' @param union If TRUE, group by label_col and union geometry, Default: `FALSE`
#' @param drop_shadow If `TRUE`, use [ggfx::with_shadow()] to add a drop shadow
#'   to the label layer with shadow_params. Defaults to `FALSE`.
#' @param shadow_params Parameters passed to [ggfx::with_shadow()] if `drop_shadow
#'   = TRUE`. Defaults to `list(x_offset = 5, y_offset = 5, sigma = 0.5)`.
#' @inheritParams sfext::st_clip
#' @inheritDotParams layer_location_data -layer_fn -from_crs
#' @seealso
#'  [sfext::st_clip()],[layer_location_data()]
#' @rdname layer_labelled
#' @aliases layer_label layer_show_label
#' @export
#' @importFrom sfext as_sf check_sf st_clip
#' @importFrom dplyr summarise
#' @importFrom sf st_union
#' @importFrom rlang as_function arg_match
#' @importFrom ggplot2 guides guide_legend aes
layer_labelled <- function(data,
                           location = NULL,
                           geom = "text",
                           fn = NULL,
                           label_col = NULL,
                           mapping = NULL,
                           union = FALSE,
                           clip = NULL,
                           dist = NULL,
                           diag_ratio = NULL,
                           unit = NULL,
                           drop_shadow = FALSE,
                           shadow_params = NULL,
                           ...) {
  if (!is_sf(data) && is.data.frame(data)) {
    # FIXME: If data is a dataframe, there should be a way of passing from_crs,
    # coords_col or other relevant parameter for conversion. Otherwise this only
    # works with data frames that match the default expectations.
    data <- sfext::as_sf(data)
  }

  data <- use_fn(data)

  sfext::check_sf(data)

  if (union) {
    # FIXME: This pattern should be added as a feature to sfext::st_union_ext
    data <- group_by_col(data, col = label_col)
    data <- dplyr::summarise(data, geometry = sf::st_union(geometry))
  }

  clip_fn <- NULL

  if (!is.null(clip)) {
    # FIXME: st_clip likely should be passed to fn for layer_location_data to
    # apply clip to any subset of the data specified by location rather than the
    # whole area
    clip_fn <-
      rlang::as_function(
        ~ sfext::st_clip(
          x = .x,
          clip = clip,
          dist = dist,
          diag_ratio = diag_ratio,
          unit = unit
        )
      )
  }

  # TODO: Duplicate with layer_location_data - consider exporting as internal
  # reference data
  ggplot_text_geoms <- c("text", "sf_text", "label", "sf_label")
  ggrepel_geoms <- c("text_repel", "label_repel")
  geomtextpath_geoms <- c("textsf", "labelsf")
  text_geoms <- c(ggplot_text_geoms, geomtextpath_geoms, ggrepel_geoms)

  geom <- rlang::arg_match(geom, text_geoms)

  label_layer <-
    layer_location_data(
      data = data,
      location = location,
      fn = clip_fn,
      mapping = mapping,
      geom = geom,
      label_col = label_col,
      ...
    )

  if (drop_shadow) {
    label_layer <- with_shadow(label_layer, shadow_params)
  }


  list(
    label_layer,
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = ggplot2::aes(label = "")
      )
    )
  )
}
