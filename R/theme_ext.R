#' Modify the text, margins, or legend for a ggplot theme
#'
#' Helper functions for modifying a ggplot theme. The "replace" and "update"
#' options for the method parameter are not currently working; keeping method =
#' NULL or method = "set" is recommended.
#'
#' @param font_family Font family, Default: 'Helvetica' If `NULL`, font_family
#'   is pulled from current set theme which is helpful for resetting all text
#'   families to the theme.
#' @param geom_text If `TRUE`, update text family for
#'   [ggplot2::geom_text()], [ggplot2::geom_sf_text()],
#'   [ggplot2::geom_label()], and [ggplot2::geom_sf_label()]
#'   to match `font_family` and color. If `FALSE`, make no changes to the theme.
#'   Default: `TRUE`.
#' @param fill Fill for `plot.background` theme element passed to
#'   [ggplot2::element_rect()] Default: `NA`.
#' @param color Color for text elements (passed to
#'   [ggplot2::element_text()] by theme_text), `plot.background`
#'   (passed to [ggplot2::element_rect()] by theme_margin). Default:
#'   `NA`.
#' @param hjust,vjust Horizontal and vertical justification.
#' @param position Legend position (“left”,“top”, “right”, “bottom”) or a
#'   two-element numeric vector to set position using Normalized Parent
#'   Coordinates ("npc"); defaults NULL
#' @param justification If `NULL`, justification is set to "center"; defaults to
#'   `NULL`. Use justification to set legend position if `inset = FALSE`.
#'   Supports "topleft", "bottomleft", "topright", or "bottomright" values.
#' @param margin Margin distance, a margin style supported by [get_margin()] or
#'   a margin object; defaults to 10.
#' @param unit Legend margin units; defaults to 'pt'.
#' @param inset If `TRUE` and position is "topleft", "bottomleft", "topright",
#'   or "bottomright", place the legend in an inset position; defaults to
#'   `TRUE`.
#' @param bgcolor Fill color for legend background; defaults to 'white'.
#' @param method Method with name of the ggplot2 geom function to use for
#'   modifying theme ("set", "update", or "replace"); defaults to `NULL`.
#' @param ... Additional parameters passed to [ggplot2::theme()].
#' @inheritParams sfext::get_paper
#' @inheritParams sfext::get_margin
#' @seealso
#'  - [ggplot2::theme()]
#'  - [ggplot2::margin()]
#'  - [ggplot2::theme_get()]
#'  - [ggplot2::update_geom_defaults()]
#' @md
#' @name theme_ext
NULL

#' @rdname theme_ext
#' @name theme_text
#' @export
#' @importFrom ggplot2 theme element_text theme_set theme_update theme_replace
#'   update_geom_defaults
theme_text <- function(font_family = NULL,
                       color = "black",
                       geom_text = TRUE,
                       hjust = NULL,
                       vjust = NULL,
                       method = NULL,
                       ...) {
  if (is.null(font_family)) {
    font_family <- ggplot2::theme_get()$text$family
  }

  if (is.null(hjust) && is.null(vjust)) {
    hjust <- 0
    vjust <- 0.5
  }

  theme <- ggplot2::theme_get()

  text_theme <-
    modifyList(
      theme,
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          family = font_family, color = color,
          hjust = hjust, vjust = vjust
        ),
        plot.subtitle = ggplot2::element_text(
          family = font_family, color = color,
          hjust = hjust, vjust = vjust
        ),
        plot.caption = ggplot2::element_text(
          family = font_family, color = color,
          hjust = hjust, vjust = vjust
        ),
        strip.text = ggplot2::element_text(
          family = font_family, color = color,
          hjust = hjust, vjust = vjust
        ),
        axis.text = ggplot2::element_text(
          family = font_family, color = color,
          hjust = hjust, vjust = vjust
        ),
        axis.title = ggplot2::element_text(
          family = font_family, color = color,
          hjust = hjust, vjust = vjust
        ),
        legend.text = ggplot2::element_text(
          family = font_family, color = color,
          hjust = hjust, vjust = vjust
        ),
        legend.title = ggplot2::element_text(
          family = font_family, color = color,
          hjust = hjust, vjust = vjust
        ),
        ...
      )
    )

  if (!is.null(method)) {
    theme_method(text_theme, method = method)

    if (geom_text) {
      ggplot2::update_geom_defaults(
        "label",
        list(family = font_family, color = color)
      )
      ggplot2::update_geom_defaults(
        "text",
        list(family = font_family, color = color)
      )
    }
  } else {
    return(text_theme)
  }
}


#' @rdname theme_ext
#' @name theme_margin
#' @export
#' @importFrom ggplot2 theme element_rect
#' @importFrom grid unit
theme_margin <- function(margin = "standard",
                         paper = NULL,
                         orientation = NULL,
                         dist = NULL,
                         unit = "in",
                         block_width = NULL,
                         header = 0,
                         footer = 0,
                         fill = NA,
                         color = NA,
                         size = 0,
                         method = NULL,
                         ...) {
  theme <- ggplot2::theme_get()

  margin_theme <-
    modifyList(
      theme,
      ggplot2::theme(
        # FIXME: Should the plot.border also be defined here?
        plot.background = ggplot2::element_rect(
          fill = fill,
          color = color,
          size = grid::unit(size, units = unit)
        ),
        plot.margin = get_margin(
          margin = margin,
          paper = paper,
          orientation = orientation,
          dist = dist,
          unit = unit,
          block_width = block_width,
          header = header,
          footer = footer
        ),
        ...
      )
    )

  if (is.null(method)) {
    return(margin_theme)
  }

  theme_method(margin_theme, method = method)
}

#' @rdname theme_ext
#' @name theme_legend
#' @param title Attributes to use for legend.title text (e.g. face and align).
#' @param nudge_inset Position adjustment in "npc" units to use for inset
#'   legends. Defaults to 0.05.
#' @export
#' @importFrom ggplot2 element_blank element_rect theme
#' @importFrom grid unit
#' @importFrom sfext get_margin
theme_legend <- function(position = NULL,
                         justification = NULL,
                         margin = 8,
                         unit = "pt",
                         inset = TRUE,
                         nudge_inset = 0.05,
                         bgcolor = "white",
                         title = list(face = "bold", align = 0),
                         method = NULL,
                         ...) {
  if ("none" %in% position) {
    legend_theme <- ggplot2::theme(legend.position = "none")
  } else {
    # TODO: Document that inset legends only work with a subset of position
    # options
    leg_pos <-
      make_legend_position(
        position = position,
        justification = justification,
        inset = inset,
        nudge_inset = nudge_inset
      )

    leg_title <- make_legend_title(title = title)

    # FIXME: This part needs a test
    # If margin is not a unit object
    if (!inherits(margin, "unit")) {
      # use a numeric margin as a dist
      if (is.numeric(margin)) {
        dist <- margin
        margin <- NULL
      } else {
        # use a character margin as a margin type
        dist <- NULL
      }

      leg_margin <- sfext::get_margin(margin = margin, dist = dist, unit = unit)
    } else {
      leg_margin <- margin
    }

    legend_theme <-
      ggplot2::theme(
        legend.position = leg_pos$position,
        legend.justification = leg_pos$justification,
        legend.box.just = leg_pos$box_justification,
        legend.title = leg_title$title,
        legend.title.align = leg_title$align_title,
        # legend.text = leg_title$text,
        legend.margin = leg_margin,
        legend.background = make_legend_bg(bgcolor),
        ...
      )
  }

  if (is.null(method)) {
    return(legend_theme)
  }

  theme_method(legend_theme, method = method)
}

#' Get position, justification, and box justification for an ggplot2 legend
#'
#' @seealso [get_legend_position_inset]
#' @param justification defaults to `NULL`.
#' @param position defaults to `NULL`.
#' @param inset If `TRUE`, return an inset legend position; defaults to `FALSE`.
#' @noRd
#' @importFrom grid unit
#' @importFrom rlang has_length
make_legend_position <- function(justification = NULL,
                                 position = NULL,
                                 inset = FALSE,
                                 nudge_inset = 0.05) {
  if (is.null(position) || !is.numeric(position)) {
    position <- match.arg(position, c(
      "left", "right", "bottom", "top",
      "topleft", "bottomleft", "topright", "bottomright", "none"
    ))
  }

  if (inset) {
    if (length(nudge_inset) == 1) {
      nudge_inset <- rep(nudge_inset, 2)
    }

    # Set default x and y position
    x_position <- 0.5
    x_justification <- "center"
    y_position <- 0.5
    y_justification <- "center"

    if (any(grepl("left", position))) {
      x_position <- nudge_inset[[1]]
      x_justification <- "left"
    } else if (any(grepl("right", position))) {
      x_position <- 1 - nudge_inset[[1]]
      x_justification <- "right"
    }

    if (any(grepl("top", position))) {
      y_position <- 1 - nudge_inset[[2]]
      y_justification <- "top"
    } else if (any(grepl("bottom", position))) {
      y_position <- nudge_inset[[2]]
      y_justification <- "bottom"
    }

    position <- grid::unit(c(x_position, y_position), unit = "npc")
    justification <- c(x_justification, y_justification)
    box_justification <-
      switch(x_justification,
        "left" = "right",
        "right" = "left"
      )
  } else {
    justification <-
      match.arg(
        justification,
        c(
          "right", "left", "bottom", "top", "center",
          "topleft", "bottomleft", "topright", "bottomright"
        ),
        several.ok = TRUE
      )

    justification <-
      switch(justification,
        "topright" = c("right", "top"),
        "topleft" = c("left", "top"),
        "bottomright" = c("right", "bottom"),
        "bottomleft" = c("left", "bottom")
      )

    if ((grepl("top", justification[[1]]) || grepl("bottom", justification[[1]]))) {
      justification <- rev(justification)
    }

    if (rlang::has_length(justification, 2) && is.null(position)) {
      position <- justification[[1]]
    }

    box_justification <- "left"

    if (any(grepl("left", justification))) {
      box_justification <- "right"
    }
  }


  list(
    "position" = position,
    "justification" = justification,
    "box_justification" = box_justification
  )
}


#' Make plot background element based on background color
#'
#' @param bgcolor Legend background color; defaults to `NULL`.
#' @noRd
#' @importFrom ggplot2 element_blank element_rect
make_legend_bg <- function(bgcolor = NULL) {
  if (is.null(bgcolor) | is.na(bgcolor)) {
    return(ggplot2::element_blank())
  }

  ggplot2::element_rect(fill = bgcolor)
}

#' @param title Named list with title face and alignment (e.g. list(face = "Georgia", align = "left"))
#' @noRd
#' @importFrom ggplot2 element_text
make_legend_title <- function(title = NULL) {
  if (is.list(title) && all(c("face", "align") %in% names(title))) {
    title <- ggplot2::element_text(face = title$face)
    # text <- ggplot2::element_text(hjust = align)
    align_title <- title$align
  } else if (is.null(title)) {
    # text <- ggplot2::element_text(hjust = align)
    # FIXME: This may break
    title <- ggplot2::element_text(face = "bold")
    align_title <- 0 # align
  }

  list(
    "title" = title,
    "align_title" = align_title
  )
}


#' @noRd
#' @importFrom ggplot2 theme_set theme_update theme_replace
theme_method <- function(x, method = NULL) {
  method <- match.arg(method, c("set", "update", "replace"))

  switch(method,
    "set" = ggplot2::theme_set(x),
    "update" = ggplot2::theme_update(x),
    "replace" = ggplot2::theme_replace(x)
  )
}
