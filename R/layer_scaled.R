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

    if (nrow(scaled_paper) > 1) {
      cli_abort(c(
        "{.arg paper}, {.arg orientation}, and {.arg scale} parameters returned multiple options.",
        "More specific parameters are required."
      ))
    }

    if (is.null(asp)) {
      asp <- scaled_paper$asp
    }

    if (!is_sf(data, ext = TRUE)) {
      cli_abort("data must be a bounding box or simple feature object.")
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

    # Compare bbox xdist and ydist to actual dimensions
    # FIXME: move this into a helper function
    ydist <- sfext::sf_bbox_ydist(bbox, units = TRUE)
    xdist <- sfext::sf_bbox_xdist(bbox, units = TRUE)

    check_xdist <- (xdist <= scaled_paper$width_actual)
    check_ydist <- (ydist <= scaled_paper$height_actual)

    fit_check <- (check_ydist && check_xdist)

    if (!clip && !fit_check) {
      cli_abort("This data covers a larger area than can be displayed at this scale ({scale}) on this paper ({paper})")
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
