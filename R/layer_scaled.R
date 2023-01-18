#' Create a ggplot2 layer scaled to a paper and orientation for a location
#'
#' Uses [layer_neatline], [sfext::standard_scales], and
#' [sfext::convert_dist_scale].
#'
#' @inheritParams sfext::convert_dist_scale
#' @inheritParams sfext::st_bbox_ext
#' @inheritParams layer_neatline
#' @param clip If `TRUE`, create scaled layer even if the data is cut off; defaults to `FALSE`.
#' @family layer
#' @name layer_scaled
#' @export
#' @importFrom sfext convert_dist_scale sf_bbox_ydist sf_bbox_xdist
#' @importFrom sf st_centroid
layer_scaled <-
  function(data = NULL,
           dist = NULL,
           diag_ratio = NULL,
           unit = NULL,
           asp = NULL,
           crs = getOption("maplayer.crs", default = 3857),
           scale = NULL,
           paper = NULL,
           orientation = NULL,
           clip = FALSE) {
    # Get paper with actual width, height, and units
    scaled_paper <-
      sfext::convert_dist_scale(
        paper = paper,
        orientation = orientation,
        scale = scale
      )

    data_units <- get_dist_units(data)

    if (nrow(scaled_paper) > 1) {
      cli::cli_warn(c(
        "{.arg paper}, {.arg orientation}, and {.arg scale} parameters
        returned multiple options.", "Using first returned option."
      ))

      scaled_paper <- scaled_paper[1, ]
    }

    asp <- asp %||% scaled_paper$asp

    if (!is_sf(data, ext = TRUE)) {
      cli::cli_abort(
        "{.arg data} must be a {.cls bbox}, {.cls sf}, or {.cls sfc} object."
        )
    }

    # Get adjusted bounding box for data
    bbox <-
      sfext::st_bbox_ext(
        x = data,
        dist = dist,
        diag_ratio = diag_ratio,
        unit = unit,
        asp = asp,
        crs = crs
      )

    if (!sfext::is_dist_units(dist)) {
      dist <- sfext::as_dist_units(dist = dist, to = data)
    }

    if (!clip && !bbox_fit_check(bbox, scaled_paper)) {
      cli_abort(
        "This data covers a larger area than can be displayed at this scale
        ({scale}) on this paper ({paper}).")
    }

    scaled_dist <- max(c(scaled_paper$width_actual, scaled_paper$height_actual)) / 2

    scaled_bbox <-
      sfext::st_bbox_ext(
        x = sf::st_centroid(data),
        dist = scaled_dist,
        asp = scaled_paper$asp,
        unit = scaled_paper$unit_actual,
        crs = crs
      )

    scaled_layer <-
      layer_neatline(
        data = scaled_bbox,
        color = NA,
        bgcolor = "none",
        expand = TRUE
      )

    return(scaled_layer)
  }



# FIXME: Replace with new sf_bbox_fit (?)

#' @noRd
#' @importFrom sfext sf_bbox_ydist sf_bbox_xdist
bbox_fit_check <- function(bbox, paper = NULL, cols = c("actual_width", "actual_height")) {
  # Compare bbox xdist and ydist to actual dimensions
  # FIXME: move this into a helper function

  units <- sfext::get_dist_units(paper[[cols[1]]])

  ydist <- sfext::sf_bbox_ydist(bbox, units = units, drop = FALSE)
  xdist <- sfext::sf_bbox_xdist(bbox, units = units, drop = FALSE)

  print(xdist)
  print(ydist)
  print(paper[[cols[1]]])
  print(paper[[cols[2]]])

  (xdist <= paper[[cols[1]]]) && (ydist <= paper[[cols[2]]])
}
