% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layer_location.R
\name{layer_location}
\alias{layer_location}
\alias{layer_show_location}
\title{Layer a location border into a ggplot2 map}
\usage{
layer_location(
  mapping = ggplot2::aes(),
  data = NULL,
  type = NULL,
  name = NULL,
  id = NULL,
  location = NULL,
  name_col = "name",
  id_col = "id",
  index = NULL,
  label = NULL,
  label_geom = NULL,
  label_col = name_col,
  union = FALSE,
  smooth = TRUE,
  crs = getOption("maplayer.crs", default = 3857),
  color = "gray40",
  linetype = "dashed",
  size = 1,
  fill = NA,
  dist = NULL,
  diag_ratio = NULL,
  unit = NULL,
  asp = NULL,
  mask = FALSE,
  neatline = FALSE,
  ...
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}} or
\code{\link[ggplot2:aes_]{aes_()}}. If specified and \code{inherit.aes = TRUE} (the
default), it is combined with the default mapping at the top level of the
plot. You must supply \code{mapping} if there is no plot mapping.}

\item{data}{Data for location to show.}

\item{type}{Type of location to return. Type can be an sf object, e.g. a data
frame with multiple neighborhoods or a character string that can be passed
to \link[getdata]{get_location_data}. If index is provided, character can also be a
character string to match the name of a list.}

\item{name}{Location name to return.}

\item{id}{Location id to return. id is coerced to character or numeric to
match the class of the id_col for type.}

\item{location}{An address, bounding box (\code{bbox}), or simple feature (\code{sf})
object passed to \link[sf:st_join]{sf::st_filter}. Any valid address or
addresses are geocoded with \link[tidygeocoder:geo]{tidygeocoder::geo}, converted to
a simple feature object, and then used as a spatial filter. \code{bbox} objects
are converted using \code{\link[sfext:sf_bbox_misc]{sfext::sf_bbox_to_sf()}}. Multiple addresses are supported.}

\item{name_col}{Column name in type with name values, Default: 'name'
Required if name provided.}

\item{id_col}{Column name in type with id values, Default: 'id'. Required if
id is provided.}

\item{index}{Optional list used to match type to data, Default: \code{NULL}}

\item{label}{label type (e.g. "text", "label")}

\item{label_col}{Column name or id for a column with the text or labels to
pass to any text geom.}

\item{union}{If \code{TRUE}, the location geometry is unioned with
\link[sf:geos_combine]{sf::st_union} and the names are combined into a single value.
Default: \code{FALSE}.}

\item{crs}{Coordinate reference system to return; defaults to NULL which
returns data using the same coordinate reference system as the provided
type of location.}

\item{color}{Color for location; defaults to "black".}

\item{linetype}{Line type for location; defaults to "dashed".}

\item{fill}{Fill for location; defaults to "NA".}

\item{dist}{buffer distance in units. Optional.}

\item{diag_ratio}{ratio of diagonal distance of area's bounding box used as
buffer distance. e.g. if the diagonal distance is 3000 meters and the
"diag_ratio = 0.1"  a 300 meter will be used. Ignored when \code{dist} is
provided.}

\item{unit}{unit to adjust location by dist or diag_ratio; defaults to
"meter"}

\item{asp}{Aspect ratio of width to height as a numeric value (e.g. 0.33) or
character (e.g. "1:3"). If numeric, \code{\link[sfext:get_asp]{get_asp()}} returns the same value
without modification.}

\item{mask}{A \code{sf} or \code{bbox} object to define the edge of the mask.
\code{diag_ratio}, \code{dist}, and \code{asp} parameters are ignored if a \code{mask} is
provided. defaults to \code{NULL}}

\item{neatline}{If \code{TRUE}, add \code{layer_neatline} with \code{expand = TRUE};
defaults to FALSE.}

\item{...}{Additional parameters passed to get_location if data is \code{NULL}.}
}
\value{
list of ggplot2 geoms
}
\description{
Helper function to make a ggplot2 layer from data returned by
\code{get_location}
}
\seealso{
\code{\link[ggplot2:ggsf]{ggplot2::CoordSf()}}
}
