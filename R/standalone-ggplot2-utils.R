# ---
# repo: elipousson/maplayer
# file: standalone-ggplot2-utils.R
# last-updated: 2023-09-15
# license: https://creativecommons.org/publicdomain/zero/1.0/
# imports: [rlang, ggplot2]
# ---
#
# nocov start

#' Combine ggplot list and vectors
#'
#' @noRd
combine_gg_list <- function(x, y = NULL) {
  if (is_bare_list(x) && ggplot2::is.ggplot(x[[1]])) {
    x <- reduce(x, function(x, gg) {x + gg})
  }

  if (is.null(y) || is_empty(y)) {
    return(x)
  }

  if (ggplot2::is.ggplot(x)) {
    return(x + y)
  }

  c(x, y)
}

#' Add a ggplot2::ggplot() object to a ggplot2 layer.
#'
#' @noRd
plot_gg_list <- function(x,
                         plot = FALSE,
                         data = NULL,
                         mapping = aes(),
                         ...,
                         call = caller_env()) {
  if (is_true(plot)) {
    init_plot <- ggplot2::ggplot(data = data, mapping = mapping, ...)

    if (is.null(x)) {
      return(init_plot)
    }

    return(init_plot + x)
  }

  if (is_false(plot) || is_null(plot)) {
    return(x)
  }

  combine_gg_list(plot, x)
}

# nocov end
