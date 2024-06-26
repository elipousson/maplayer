#' Map icons
#'
#' @description An index of map icons from seven sources:
#' - [mapbox/maki](https://github.com/mapbox/maki)
#' - [ideditor/temaki](https://github.com/rapideditor/temaki)
#' - [manifestinteractive/weather-underground-icons](https://github.com/manifestinteractive/weather-underground-icons/)
#' - [openstreetmap/map-icons](https://github.com/openstreetmap/map-icons/)
#' - [openstreetmap/lane-icons](https://github.com/openstreetmap/lane-icons/)
#' - [Esri/calcite-point-symbols](https://github.com/Esri/calcite-point-symbols)
#' - [NPMap Symbol Library](https://github.com/nationalparkservice/symbol-library/)
#'
#' Most of these icon sources use open licenses.
#' [Maki](https://github.com/mapbox/maki/blob/main/LICENSE.txt),
#' [Temaki](https://github.com/rapideditor/temaki/blob/main/LICENSE.md), and the
#' [OSM lane icons](https://github.com/openstreetmap/lane-icons/blob/master/LICENSE.md)
#' all use a CC0 license. The Weather Underground Icons use an
#' [MIT
#' license](https://github.com/manifestinteractive/weather-underground-icons/blob/master/LICENSE).
#' The OSM map icons use an unspecified PD style license.
#' The Calcite icons are [available under the Esri Master License Agreement
#' (MLA)](https://github.com/Esri/calcite-point-symbols#licensing). The NPMap
#' Symbol Library is created by the National Park Service so is assumed to be a
#' [Public Domain work in the U.S.](http://www.usa.gov/publicdomain/label/1.0/).
#'
#' This index was last updated on 2024-04-08 and may not include any icons
#' that have been added since that date.
#'
#' @details
#' The name column is not unique so a px or source may be required when using
#' [layer_icon()].
#'
#' @format A tibble with 5,036 rows and 5 variables:
#' \describe{
#'   \item{`name`}{Icon name}
#'   \item{`url`}{Icon URL on GitHub repo}
#'   \item{`style`}{Icon style (blank if not applicable)}
#'   \item{`size`}{Icon width/height (pixels)}
#'   \item{`repo`}{GitHub repository for icon collection}
#' }
"map_icons"
