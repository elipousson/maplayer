# ---
# repo: elipousson/maplayer
# file: standalone-ggplot2-utils.R
# last-updated: 2024-05-21
# license: https://creativecommons.org/publicdomain/zero/1.0/
# dependencies: standalone-eval_tidy_fn.R
# imports: [rlang, ggplot2, glue]
# ---
# ## Changelog
#
# 2024-05-21:
# - Add {glue} to imports and `standalone-eval_tidy_fn.R` to dependencies.
# - Add `gg_plot_layers()`
# - Add `gg_labs()` and `gg_caption()`
#
# 2023-09-15:
# - Initial setup
#
# nocov start

#' Combine ggplot list and vectors
#' @noRd
combine_gg_list <- function(x, y = NULL) {
  if (is_bare_list(x) && ggplot2::is.ggplot(x[[1]])) {
    x <- reduce(x, function(x, gg) {
      x + gg
    })
  }

  if (is.null(y) || is_empty(y)) {
    return(x)
  }

  if (ggplot2::is.ggplot(x)) {
    return(x + y)
  }

  c(x, y)
}

#' Add a [ggplot2::ggplot()] object to a ggplot2 layer.
#' @noRd
plot_gg_list <- function(x = NULL,
                         plot = FALSE,
                         data = NULL,
                         mapping = ggplot2::aes(),
                         ...,
                         call = caller_env()) {
  if (is_false(plot) || is_null(plot)) {
    return(x)
  }

  if (is_true(plot)) {
    plot <- ggplot2::ggplot(data = data, mapping = mapping, ...)
    return(plot + x)
  }

  combine_gg_list(plot, x)
}

#' Make ggplot2 plot from layers
#' @keywords internal ggplot2
#' @noRd
gg_plot_layers <- function(plot,
                           ...,
                           .bg_layer = NULL,
                           .layer = NULL,
                           .fg_layer = NULL,
                           labs_params = NULL,
                           labs_fn = gg_labs,
                           save = FALSE,
                           save_params = NULL,
                           save_fn = ggplot2::ggsave,
                           call = caller_env()) {
  if (!ggplot2::is.ggplot(plot)) {
    plot <- plot_gg_list(plot = plot, ...)
  }

  if (!is_empty(labs_params)) {
    plot <- plot +
      eval_tidy_fn(
        params = labs_params,
        fn = labs_fn,
        call = call
      )
  }

  plot <- combine_gg_list(plot, .bg_layer)

  plot <- combine_gg_list(plot, .layer)

  plot <- combine_gg_list(plot, .fg_layer)

  if (save && !is_empty(save_params)) {
    eval_tidy_fn(
      plot,
      params = save_params,
      fn = save_fn,
      call = call
    )
  }

  plot
}

#' Add labels to a ggplot2 plot or map
#'
#' A helper function that converts strings to glue strings for the title,
#' subtitle, and caption.
#'
#' @inheritParams ggplot2::labs
#' @inheritDotParams ggplot2::labs
#' @param source_note Data source(s) to append to caption or use as caption (if
#'   no caption is supplied). Also supports glue string interpolation.
#' @param source_sep,source_before,source_end Strings used to separate caption
#'   (if supplied) and source note, add before the source note, and add after
#'   the source note.
#' @inheritParams glue::glue
#' @noRd
gg_labs <- function(...,
                    title = ggplot2::waiver(),
                    subtitle = ggplot2::waiver(),
                    caption = ggplot2::waiver(),
                    tag = ggplot2::waiver(),
                    alt = ggplot2::waiver(),
                    alt_insight = ggplot2::waiver(),
                    source_note = NULL,
                    source_sep = ". ",
                    source_before = "Source: ",
                    source_end = ".",
                    collapse = " ",
                    .sep = "",
                    .envir = parent.frame(),
                    .open = "{",
                    .close = "}",
                    .na = "NA",
                    .null = character(),
                    .comment = "#",
                    .literal = FALSE,
                    .transformer = identity_transformer,
                    .trim = TRUE) {
  if (!is.null(source_note)) {
    caption <- gg_caption(
      caption,
      source_note,
      source_sep = source_sep,
      before = source_before,
      end = source_end,
      collapse = collapse,
      .sep = .sep,
      .envir = .envir,
      .open = .open,
      .close = .close,
      .na = .na,
      .null = .null,
      .comment = .comment,
      .literal = .literal,
      .transformer = .transformer,
      .trim = .trim
    )
  }

  labs_params <- rlang::list2(
    ...,
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag,
    alt = alt,
    alt_insight = alt_insight
  )

  labs_params <- labs_params[
    vapply(
      labs_params,
      function(x) {
        !.is_waiver(x) & !is.null(x)
      },
      TRUE
    )
  ]

  labs_params <- lapply(
    labs_params,
    function(x) {
      glue::glue(
        x,
        .sep = .sep,
        .envir = .envir,
        .open = .open,
        .close = .close,
        .na = .na,
        .null = .null,
        .comment = .comment,
        .literal = .literal,
        .transformer = .transformer,
        .trim = .trim
      )
    }
  )

  rlang::exec(ggplot2::labs, !!!labs_params)
}

#' @noRd
.is_waiver <- function(x) {
  identical(x, ggplot2::waiver())
}

#' @noRd
gg_caption <- function(caption = ggplot2::waiver(),
                       source_note = NULL,
                       source_sep = ". ",
                       before = "Source: ",
                       after = ".",
                       collapse = " ",
                       .sep = "",
                       .envir = parent.frame(),
                       .open = "{",
                       .close = "}",
                       .na = "NA",
                       .null = character(),
                       .comment = "#",
                       .literal = FALSE,
                       .transformer = identity_transformer,
                       .trim = TRUE) {
  if (is.null(source_note)) {
    return(caption)
  }

  if (is.null(caption) || .is_waiver(caption)) {
    caption <- paste0(before, source_note, after)
    source_note <- NULL
  } else {
    source_note <- paste0(source_sep, before, source_note, after)
  }

  if (length(caption) > 1) {
    caption <- paste0(caption, collapse = collapse)
  }

  glue::glue(
    caption,
    source_note,
    .sep = .sep,
    .envir = .envir,
    .open = .open,
    .close = .close,
    .na = .na,
    .null = .null,
    .comment = .comment,
    .literal = .literal,
    .transformer = .transformer,
    .trim = .trim
  )
}
# nocov end
