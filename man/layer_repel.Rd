% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layer_repel.R
\name{layer_repel}
\alias{layer_repel}
\alias{geom_sf_label_repel}
\alias{geom_sf_text_repel}
\title{Use ggrepel to create text annotations based on simple features}
\usage{
layer_repel(
  mapping = aes(),
  data = NULL,
  label_col = "name",
  geom = c("text", "label"),
  location_lims = NULL,
  xlim = c(NA, NA),
  ylim = c(NA, NA),
  ...
)

geom_sf_label_repel(mapping = aes(), data = NULL, label_col = "name", ...)

geom_sf_text_repel(mapping = aes(), data = NULL, label_col = "name", ...)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2]{aes}} or
\code{\link[ggplot2]{aes_}}. If specified and \code{inherit.aes = TRUE} (the
default), is combined with the default mapping at the top level of the
plot. You only need to supply \code{mapping} if there isn't a mapping
defined for the plot.}

\item{data}{A data frame. If specified, overrides the default data frame
defined at the top level of the plot.}

\item{label_col}{Column name to use for label aesthetic mapping. Optional if
label is provided to mapping; required otherwise.}

\item{geom}{Character vector with geom to use, "text" for
\code{\link[ggrepel:geom_text_repel]{ggrepel::geom_text_repel()}} or "label" for \code{\link[ggrepel:geom_text_repel]{ggrepel::geom_label_repel()}}.}

\item{location_lims}{A \code{sf}, \code{sfc}, or \code{bbox} object to use in setting xlim
and ylim values if no xlim and ylim value area provided. Using this
parameter constrains labels to the bounding box of \code{location_lims}.}

\item{xlim, ylim}{Limits for the x and y axes. Text labels will be constrained
to these limits. By default, text labels are constrained to the entire plot
area.}

\item{...}{
  Arguments passed on to \code{\link[ggrepel:geom_text_repel]{ggrepel::geom_label_repel}}
  \describe{
    \item{\code{position}}{Position adjustment, either as a string, or the result of
a call to a position adjustment function.}
    \item{\code{parse}}{If TRUE, the labels will be parsed into expressions and
displayed as described in ?plotmath}
    \item{\code{box.padding}}{Amount of padding around bounding box, as unit or number.
Defaults to 0.25. (Default unit is lines, but other units can be specified
by passing \code{unit(x, "units")}).}
    \item{\code{label.padding}}{Amount of padding around label, as unit or number.
Defaults to 0.25. (Default unit is lines, but other units can be specified
by passing \code{unit(x, "units")}).}
    \item{\code{point.padding}}{Amount of padding around labeled point, as unit or
number. Defaults to 0. (Default unit is lines, but other units can be
specified by passing \code{unit(x, "units")}).}
    \item{\code{label.r}}{Radius of rounded corners, as unit or number. Defaults
to 0.15. (Default unit is lines, but other units can be specified by
passing \code{unit(x, "units")}).}
    \item{\code{label.size}}{Size of label border, in mm.}
    \item{\code{min.segment.length}}{Skip drawing segments shorter than this, as unit or
number. Defaults to 0.5. (Default unit is lines, but other units can be
specified by passing \code{unit(x, "units")}).}
    \item{\code{arrow}}{specification for arrow heads, as created by \code{\link[grid]{arrow}}}
    \item{\code{force}}{Force of repulsion between overlapping text labels. Defaults
to 1.}
    \item{\code{force_pull}}{Force of attraction between a text label and its
corresponding data point. Defaults to 1.}
    \item{\code{max.time}}{Maximum number of seconds to try to resolve overlaps.
Defaults to 0.5.}
    \item{\code{max.iter}}{Maximum number of iterations to try to resolve overlaps.
Defaults to 10000.}
    \item{\code{max.overlaps}}{Exclude text labels when they overlap too many other
things. For each text label, we count how many other text labels or other
data points it overlaps, and exclude the text label if it has too many overlaps.
Defaults to 10.}
    \item{\code{nudge_x,nudge_y}}{Horizontal and vertical adjustments to nudge the
starting position of each text label. The units for \code{nudge_x} and
\code{nudge_y} are the same as for the data units on the x-axis and y-axis.}
    \item{\code{xlim,ylim}}{Limits for the x and y axes. Text labels will be constrained
to these limits. By default, text labels are constrained to the entire plot
area.}
    \item{\code{na.rm}}{If \code{FALSE} (the default), removes missing values with
a warning.  If \code{TRUE} silently removes missing values.}
    \item{\code{show.legend}}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.}
    \item{\code{direction}}{"both", "x", or "y" -- direction in which to adjust position of labels}
    \item{\code{seed}}{Random seed passed to \code{\link[base]{set.seed}}. Defaults to
\code{NA}, which means that \code{set.seed} will not be called.}
    \item{\code{verbose}}{If \code{TRUE}, some diagnostics of the repel algorithm are printed}
    \item{\code{inherit.aes}}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2]{borders}}.}
  }}
}
\description{
Use \link[ggrepel:geom_text_repel]{ggrepel::geom_label_repel} or \link[ggrepel:geom_text_repel]{ggrepel::geom_text_repel} with
\code{\link[ggplot2:stat_sf_coordinates]{ggplot2::stat_sf_coordinates()}} to create a layer of textual annotations
repelled from simple feature locations.
}
