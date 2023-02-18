#' Use ggforce to create an annotation layer using simple feature data
#'
#' Use [ggforce::geom_mark_rect()], [ggforce::geom_mark_circle()],
#' [ggforce::geom_mark_ellipse()], or [ggforce::geom_mark_hull()] to annotate
#' simple feature objects. This function modifies the geometry and sets the stat
#' to "st_coordinates" to make it easy to use these annotation geoms with simple
#' feature objects.
#'
#' @section Geometry conversion for MULTIPOLYGON or POLYGON data:
#'
#' If cast is `FALSE` and the data geometry type is MULTIPOLYGON or POLYGON, the
#' annotation uses a centroid for the annotation geometry. If cast is `TRUE`,
#' the data is converted to POINT geometry using [sfext::st_cast_ext()] and the
#' modified geometry passed on to selected geom.
#'
#' @section Setting label and description:
#'
#' Labels and descriptions can be included in the aesthetic mapping for the
#' layer consistent with the standard documented approaches for all four
#' functions.
#'
#'   Labels and descriptions also can be set in two non-standard ways:
#'
#'   - Setting label_col and/or or desc_col to character strings with the column
#'   names for labels and/or descriptions
#'   - Setting label_col and/or desc_col with the value of the label/description
#'
#' If the first approach is used, label_col and desc_col can also can be created
#' or modified by a function provided to the fn parameter. Otherwise, the
#' columns must be present in the data to work. If the second approach is used,
#' the length and order of vector provided to label_col and/or desc_col must
#' match that length and order of the data (after the fn is applied).
#'
#' @param data A `sf`, `sfc`, or `bbox` object that can be converted with
#'   [sfext::as_sf]
#' @param fn Function to apply to data before passing to geom, typically a
#'   predicate or filter to define area for annotation. A filter can also be
#'   passed to any of the {ggforce} functions using the filter aesthetic.
#'   Default: NULL
#' @param mapping Aesthetic mapping to pass to geom, Default: `NULL`
#' @param center If `FALSE`, use [sfext::st_cast_ext] MULTIPOLYGON and
#'   POLYGON data to POINT; If `TRUE`, use [sfext::st_center] use centroid as
#'   the feature geometry. Defaults to FALSE.
#' @param desc_col Column name to use for description. Defaults to `NULL`.
#' @inheritParams ggforce::geom_mark_circle
#' @param geom geom to use for layer (options include "rect", "circle",
#'   "ellipse", or "hull"), Default: `NULL`
#' @param font_family,font_face,font_color Parameters passed to label.family,
#'   label.fontface, and label.colour. If `NULL`, values are set to match
#'   [ggplot2::geom_label] defaults. Defaults to `NULL`.
#' @inheritParams layer_labelled
#' @param stat stat to pass to ggforce function; defaults to "sf_coordinates"
#' @param ... Additional parameters passed to [ggforce::geom_mark_rect()],
#'   [ggforce::geom_mark_circle()], [ggforce::geom_mark_ellipse()], or
#'   [ggforce::geom_mark_hull()] or on to [ggplot2::layer()]
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # Mark the 12th Council District on a Baltimore neighborhood basemap
#'   ggplot::ggplot() +
#'     layer_location_data(
#'       data = "neighborhoods",
#'       package = "mapbaltimore"
#'     ) +
#'     layer_marked(
#'       data = getdata::get_location(
#'         type = "council district",
#'         name = "District 12",
#'         package = "mapbaltimore"
#'       ),
#'       geom = "hull",
#'       color = "red",
#'       size = 1.5
#'     )
#' }
#' }
#' @seealso
#'  - [ggforce::geom_mark_rect()]
#'  - [ggforce::geom_mark_circle()]
#'  - [ggforce::geom_mark_ellipse()]
#'  - [ggforce::geom_mark_hull()]
#' @rdname layer_marked
#' @aliases layer_show_mark
#' @export
#' @importFrom ggplot2 unit
#' @importFrom sfext as_sf st_center st_cast_ext
#' @importFrom sf st_geometry
layer_marked <- function(data,
                         fn = NULL,
                         mapping = NULL,
                         label_col = NULL,
                         desc_col = NULL,
                         geom = NULL,
                         center = FALSE,
                         font_family = NULL,
                         font_face = c("bold", "plain"),
                         font_color = NULL,
                         expand = ggplot2::unit(5, "mm"),
                         radius = expand,
                         stat = "sf_coordinates",
                         drop_shadow = FALSE,
                         shadow_params = list(x_offset = 5, y_offset = 5, sigma = 0.5, ...),
                         ...) {
  rlang::check_installed("ggforce")

  # FIXME: This should be consistent across the different layer functions
  data <- sfext::as_sf(data)

  if (!is.null(fn)) {
    data <- use_fn(data, fn)
  }

  # Get default values for geom
  label_default_aes <-
    get(
      "GeomLabel",
      envir = asNamespace("ggplot2"),
      mode = "any"
    )[["default_aes"]]

  # Set font_family, font_face, and font_color to default values if NULL
  if (is.null(font_family)) {
    font_family <-
      label_default_aes[["family"]]
  }

  if (is.null(font_face)) {
    font_face <-
      label_default_aes[["fontface"]]
  }

  if (is.null(font_color)) {
    font_color <-
      label_default_aes[["colour"]]
  }

  # Add label_col to data if not present
  data <- add_col(data = data, col = label_col)

  # Add desc_col to data if not present
  data <- add_col(data = data, col = desc_col)

  # Add label and description to mapping
  mapping <-
    modify_mapping(mapping = mapping, label = label_col, description = desc_col)

  # Add geometry to mapping
  mapping <-
    modify_mapping(mapping = mapping, data = data)

  if (center) {
    sf::st_geometry(data) <-
      sfext::st_center(x = data)$sfc
  } else {
    data <-
      sfext::st_cast_ext(x = data)
  }

  geom <- match.arg(geom, c("rect", "circle", "ellipse", "hull"))

  mark_layer <-
    switch(geom,
      # Annotate areas with rectangles
      "rect" = ggforce::geom_mark_rect(
        data = data, mapping = mapping,
        label.family = font_family,
        label.fontface = font_face,
        label.colour = font_color,
        stat = stat,
        ...
      ),
      # Annotate areas with circles
      "circle" = ggforce::geom_mark_circle(
        data = data, mapping = mapping,
        label.family = font_family,
        label.fontface = font_face,
        label.colour = font_color,
        expand = expand,
        radius = radius,
        stat = stat,
        ...
      ),
      # Annotate areas with ellipses
      "ellipse" = ggforce::geom_mark_ellipse(
        data = data, mapping = mapping,
        label.family = font_family,
        label.fontface = font_face,
        label.colour = font_color,
        expand = expand,
        radius = radius,
        stat = stat,
        ...
      ),
      # Annotate areas with hulls
      "hull" = ggforce::geom_mark_hull(
        data = data, mapping = mapping,
        label.family = font_family,
        label.fontface = font_face,
        label.colour = font_color,
        expand = expand,
        radius = radius,
        stat = stat,
        ...
      )
    )

  if (!drop_shadow) {
    return(mark_layer)
  }

  with_shadow(
    mark_layer,
    params = shadow_params
  )
}
