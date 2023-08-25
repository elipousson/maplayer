#' Convert a set of ggplot2 maps to an atlas
#'
#' [make_atlas()] is a wrapper for [papersize::page_layout()] and
#' [papersize::map_ggsave_ext()] with the intent for taking a list of maps into
#' a set of patchwork plots and optionally save plots to file. The function is
#' similar to [papersize::make_contact_sheets()].
#'
#' @param plots A list of ggplot2 maps to assemble into a set of sheet maps in
#'   an atlas format.
#' @inheritParams papersize::get_page_size
#' @inheritParams papersize::page_layout
#' @inheritParams papersize::map_ggsave_ext
#' @inheritDotParams papersize::map_ggsave_ext -plot
#' @param save If `TRUE`, save atlas plots to files using
#'   [papersize::map_ggsave_ext()] Default: FALSE
#' @returns OUTPUT_DESCRIPTION
#' @details DETAILS
#' @example examples/make_atlas.R
#' @rdname make_atlas
#' @keywords internal
#' @export
#' @importFrom papersize get_page_size page_layout map_ggsave_ext
#' @importFrom cli cli_progress_step
make_atlas <- function(plots,
                       dims = NULL,
                       ncol = NULL,
                       nrow = NULL,
                       page = "letter",
                       orientation = "portrait",
                       save = FALSE,
                       filename = NULL,
                       ...) {
  page <- papersize::get_page_size(page, orientation = orientation)

  cli::cli_progress_step("Creating sheet map plots")

  sheets <- papersize::page_layout(
      plots = plots,
      page = page,
      dims = dims,
      ncol = ncol,
      nrow = nrow
    )

  if (!save) {
    return(sheets)
  }

  cli::cli_progress_step("Saving sheet maps to file")

  papersize::map_ggsave_ext(
    plot = sheets,
    paper = page,
    filename = filename,
    ...
  )
}
