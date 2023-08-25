#' Layer for counting occurrences of data in spatial relation a location or
#' other sf object
#'
#' Wraps [sfext::count_sf_ext()]. Specification of parameters for this function
#' may be too complex and may be changed in the future.
#'
#' @param location Passed to x parameter of [sfext::count_sf_ext()].
#' @inheritParams sfext::count_sf_ext
#' @inheritDotParams sfext::count_sf_ext -x
#' @param grid_params Passed to [layer_location_data()] to style foreground grid
#'   with fill based on count.
#' @param show_data  If `TRUE`, add background layer with data to stack returned
#'   by function. If `TRUE` and grid_params includes a fixed aesthetic for
#'   alpha, divide alpha in half to ensure background data is visible below the
#'   filled grid.
#' @param data_params Passed to [layer_location_data()] to style background
#'   layer based on data.
#' @param show_label If `TRUE`, add layer with labels to stack returned by
#'   function.
#' @param label_params Passed to [layer_labelled()] for foreground labels with
#'   fill based on count.
#' @param scale_fn,scale_params Scale function and parameters. Defaults to
#'   [ggplot2::scale_fill_continuous()].
#' @examples
#'
#' nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
#' data <- sf::st_sample(nc, 75)
#'
#' ggplot() +
#'   layer_count(data = data, location = nc)
#'
#' ggplot() +
#'   layer_count(data = data, y = nc, .id = "FIPS")
#'
#' @export
#' @importFrom sf st_intersects
#' @importFrom ggplot2 scale_fill_continuous
#' @importFrom scales breaks_pretty
#' @importFrom sfext count_sf_ext
layer_count <- function(data,
                        location = NULL,
                        y = NULL,
                        join = sf::st_intersects,
                        largest = TRUE,
                        replace_na = FALSE,
                        lims = NULL,
                        .id = "id",
                        grid_params = list(
                          alpha = 1,
                          color = NA
                        ),
                        show_data = FALSE,
                        data_params = list(
                          mapping = aes(),
                          alpha = 0.75,
                          size = 1
                        ),
                        show_label = FALSE,
                        label_params = NULL,
                        scale_fn = ggplot2::scale_fill_continuous,
                        scale_params = list(
                          type = "viridis",
                          breaks = scales::breaks_pretty(n = 4)
                        ),
                        ...) {
  check_installed("lwgeom")

  count_data <- suppressWarnings(
      sfext::count_sf_ext(
        data = data,
        x = location,
        y = y,
        join = join,
        largest = largest,
        replace_na = replace_na,
        lims = lims,
        .id = .id,
        ...
      )
    )

  layer_stack <- list(NULL)

  if (show_data) {
    data_params$mapping <- data_params$mapping %||% aes()

    layer_stack <-
      list(
        eval_tidy_fn(
          x = data,
          fn = layer_location_data,
          params = data_params
        )
      )

    if (!is.null(grid_params$alpha)) {
      grid_params$alpha <- grid_params$alpha / 2
    }
  }

  grid_params$mapping <- grid_params$mapping %||%
    aes(fill = .data[["n"]])

  layer_stack <- c(
      layer_stack,
      list(
        eval_tidy_fn(
          x = count_data,
          params = grid_params,
          fn = layer_location_data
        ),
        eval_tidy_fn(
          fn = scale_fn,
          params = scale_params
        )
      )
    )

  if (!show_label) {
    return(layer_stack)
  }

  label_params$mapping <- aes_label(
      mapping = label_params$mapping %||% aes(),
      data = count_data,
      label_col = "n"
    )

  c(
    layer_stack,
    eval_tidy_fn(
      x = count_data,
      fn = layer_labelled,
      params = label_params
    )
  )
}
