#' Add labels to a ggplot2 plot or map
#'
#' A helper function that converts strings to glue strings for the title,
#' subtitle, and caption. In progress.
#'
#' @inheritParams ggplot2::labs
#' @param location sf or bbox object or character string
#' @param name_col Column name holding name or identifier for distinct places
#'   within the simple feature collection provided to location. Not supported
#'   for bbox objects.
#' @param fill,color,size,shape,alpha,linewidth Optional labels for mapped
#'   aesthetics. Defaults to [ggplot2::waiver()].
#' @param source Data source(s). Not yet used or supported by function.
#' @param alt Text used for the generation of alt-text for the plot.
#' @param .na,.null Additional parameters passed to [glue::glue()]
#' @param .envir Environment passed to [glue::glue()]; defaults to
#'   [parent.frame()].
#' @param use_md If `TRUE`, use the ggtext::element_markdown() element for the
#'   plot title, subtitle, and caption. Defaults to `FALSE`.
#' @export
#' @importFrom ggplot2 labs waiver
labs_ext <- function(title = ggplot2::waiver(),
                     subtitle = ggplot2::waiver(),
                     caption = ggplot2::waiver(),
                     tag = ggplot2::waiver(),
                     alt = ggplot2::waiver(),
                     fill = ggplot2::waiver(),
                     color = ggplot2::waiver(),
                     size = ggplot2::waiver(),
                     shape = ggplot2::waiver(),
                     alpha = ggplot2::waiver(),
                     linewidth = ggplot2::waiver(),
                     location = NULL,
                     name_col = NULL, # Check param name
                     source = NULL,
                     .na = "NA",
                     .null = NULL,
                     .envir = parent.frame(),
                     use_md = FALSE,
                     ...) {
  labs <- ggplot2::labs(
    title = glue(title, .na = .na, .null = .null, .envir = .envir),
    subtitle = glue(subtitle, .na = .na, .null = .null, .envir = .envir),
    caption = glue(caption, .na = .na, .null = .null, .envir = .envir),
    tag = glue(tag, .na = .na, .null = .null, .envir = .envir),
    alt = glue(alt, .na = .na, .null = .null, .envir = .envir),
    fill = glue(fill, .na = .na, .null = .null, .envir = .envir),
    color = glue(color, .na = .na, .null = .null, .envir = .envir),
    size = glue(size, .na = .na, .null = .null, .envir = .envir),
    shape = glue(shape, .na = .na, .null = .null, .envir = .envir),
    alpha = glue(alpha, .na = .na, .null = .null, .envir = .envir),
    linewidth = glue(linewidth, .na = .na, .null = .null, .envir = .envir),
    ...
  )

  if (!use_md) {
    return(labs)
  }

  check_installed("ggtext")

  list(
    labs,
    ggplot2::theme(
      plot.title = ggtext::element_markdown(),
      plot.subtitle = ggtext::element_markdown(),
      plot.caption = ggtext::element_markdown()
    )
  )
}
