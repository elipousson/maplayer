#' Make group layers
#'
#' Can be used to make multiple layers or multiple maps based on a grouping
#' variable.
#'
#' Scales are applied a palette and aesthetic are provided and basemap is set to
#' `TRUE`.
#'
#' @inheritParams layer_location_data
#' @param groupname_col Group column name. Defaults to "group".
#' @param aesthetics Aesthetic to map to groupname_col. Defaults to "fill"; also
#'   supports "color" or c("fill", "color").
#' @param ... Additional parameters passed to [layer_location_data()]
#' @rdname layer_grouped
#' @aliases layer_group_data
#' @export
#' @importFrom dplyr group_by group_nest
#' @importFrom purrr map
#' @importFrom ggplot2 ggplot
layer_grouped <- function(data,
                          mapping = NULL,
                          groupname_col = "group",
                          label_col = "name",
                          geom = "sf",
                          basemap = FALSE,
                          palette = NULL,
                          aesthetics = "fill",
                          ...) {
  geom_type <- sfext::is_geom_type(x = data)

  data <- group_by_col(data = data, col = groupname_col)
  nested <- dplyr::group_nest(data, keep = TRUE)

  if ((geom_type$POINTS || geom_type$LINESTRINGS) && !("color" %in% aesthetics)) {
    cli_warn("This data has {.val {geom_type$TYPES}} geometry which is typically used with a 'color' aesthetic mapping.")
  } else if (geom_type$POLYGONS && !("fill" %in% aesthetics)) {
    cli_warn("This data has {.val {geom_type$TYPES}} geometry which is typically used with a 'fill' aesthetic mapping.")
  }

  if (("color" %in% aesthetics) && !("color" %in% names(mapping))) {
    mapping <- modify_mapping(mapping = mapping, color = groupname_col)
  }

  if (("fill" %in% aesthetics) && !("fill" %in% names(mapping))) {
    mapping <- modify_mapping(mapping = mapping, fill = groupname_col)
  }

  layer_params <- list2(...)

  group_layers <-
    purrr::map(
      nested$data,
      ~ layer_location_data(
        data = .x,
        mapping = mapping,
        geom = geom,
        label_col = label_col,
        !!!layer_params
      )
    )

  if (basemap) {
    group_layers <- purrr::map(
      group_layers,
      ~ ggplot2::ggplot() +
        .x
    )
  }

  # Create scale based on detail_df and groupname_col
  if (!is.null(groupname_col) && !is.null(palette)) {
    group_scale <-
      scale_group_data(
        data = dplyr::bind_rows(nested$data),
        groupname_col = groupname_col,
        palette = palette,
        aesthetics = aesthetics
      )

    if (basemap) {
      # Combine detail maps with scale and legend
      group_layers <-
        suppressMessages(
          purrr::map(
            group_layers,
            ~ .x +
              group_scale
          )
        )
    }
  }

  group_layers
}
