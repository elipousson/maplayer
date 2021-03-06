
<!-- README.md is generated from README.Rmd. Please edit that file -->

# maplayer <a href="https://elipousson.github.io/maplayer/"><img src="man/figures/logo.png" align="right" height="128" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/maplayer)](https://CRAN.R-project.org/package=maplayer)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Codecov test
coverage](https://codecov.io/gh/elipousson/maplayer/branch/main/graph/badge.svg)](https://app.codecov.io/gh/elipousson/maplayer?branch=main)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

The goal of maplayer is to provide a consistent set of functions for
creating map layers using simple feature ({sf}) data, the {ggplot2}
package, and a variety of ggplot2 extension packages.

## Basic functions

The main layers that work with both a data and location sf object are:

-   `layer_location_data()`
-   `layer_location()`
-   `layer_location_context()`

There are several layers that can work with a single sf object as the
input data:

-   `layer_markers()`
-   `layer_numbers()`
-   `layer_frame()`
-   `layer_scaled()`
-   `layer_mask()`
-   `layer_neatline()`

Finally, there are layers that require additional packages (listed in
Suggests) or an API key (for `layer_mapbox`):

-   `layer_labelled()` optionally uses
    [ggrepel](https://ggrepel.slowkow.com/) or
    [geomtextpath](https://allancameron.github.io/geomtextpath/)
-   `layer_mapbox()` uses
    [mapboxapi](https://walker-data.com/mapboxapi/)
-   `layer_marked()` uses [ggforce](https://ggforce.data-imaginist.com/)
-   `layer_icon()` uses
    [ggsvg](https://coolbutuseless.github.io/package/ggsvg/)

The package also allows the optional use of packages designed for
transforming spatial data or modifying ggplot2 maps. These include:

-   [smoothr](https://strimas.com/smoothr/) (required to use the smooth
    parameter)
-   [ggfx](https://ggfx.data-imaginist.com/) (required to use the
    drop_shadow parameter)

Many of the functions in {maplayer} were originally developed for the
{overedge} package or before that for the {mapbaltimore} package. The
{overedge} package has been split up into three smaller packages
including {maplayer},
[{getdata}](https://elipousson.github.io/getdata/), and
[{sfext}](https://elipousson.github.io/sfext/).

## Installation

You can install the development version of maplayer like so:

``` r
pak::pkg_install("elipousson/maplayer")
```

## Example

``` r
library(maplayer)
library(ggplot2)
library(sf)
#> Linking to GEOS 3.9.1, GDAL 3.4.2, PROJ 8.2.1; sf_use_s2() is TRUE
```

### Make icon maps with sf objects and ggplot2

`layer_icon()` wraps `ggsvg::geom_point_svg()` to provide an convenient
way to make icon maps.

You can create maps using a single named icon that matches one of the
icons in `map_icons`.

``` r
nc <- st_read(system.file("shape/nc.shp", package = "sf"))
#> Reading layer `nc' from data source 
#>   `/Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/library/sf/shape/nc.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27
nc <- st_transform(nc, 3857)
theme_set(theme_void())

nc_map <-
  ggplot() +
  geom_sf(data = nc)

nc_map +
  layer_icon(data = nc, icon = "point-start", size = 8)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />
