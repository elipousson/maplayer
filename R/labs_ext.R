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
#' @export
#' @importFrom ggplot2 labs waiver
labs_ext <- function(...,
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
                     .sep = "",
                     .envir = parent.frame(),
                     .open = "{",
                     .close = "}",
                     .na = "NA",
                     .null = character(),
                     .comment = "#",
                     .literal = FALSE,
                     .transformer = glue::identity_transformer,
                     .trim = TRUE) {
  gg_labs(
    ...,
    title = title,
    subtitle = subtitle,
    caption = caption,
    tag = tag,
    alt = alt,
    alt_insight = alt_insight,
    source_note = source_note,
    source_sep = source_sep,
    source_before = source_before,
    source_end = source_end,
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
