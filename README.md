
<!-- README.md is generated from README.Rmd. Please edit that file -->

# maplayer <a href="https://elipousson.github.io/maplayer/"><img src="man/figures/logo.png" align="right" height="128"/></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/elipousson/maplayer/branch/main/graph/badge.svg)](https://app.codecov.io/gh/elipousson/maplayer?branch=main)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

The goal of maplayer is to provide a consistent set of functions for
creating map layers using simple feature
([{sf}](https://r-spatial.github.io/sf/)) data, the
[{ggplot2}](https://ggplot2.tidyverse.org/) package, and a variety of
ggplot2 extension packages.

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
Suggests):

-   `layer_labelled()` optionally uses
    [{ggrepel}](https://ggrepel.slowkow.com/) or
    [{geomtextpath}](https://allancameron.github.io/geomtextpath/)
-   `layer_mapbox()` uses
    [{mapboxapi}](https://walker-data.com/mapboxapi/) (requires an API
    key)
-   `layer_marked()` uses
    [{ggforce}](https://ggforce.data-imaginist.com/)
-   `layer_icon()` uses
    [{ggsvg}](https://coolbutuseless.github.io/package/ggsvg/)
-   `layer_inset()` uses
    [{patchwork}](https://patchwork.data-imaginist.com/) or optionally
    [{figpatch}](https://bradyajohnston.github.io/figpatch/) (for
    `stamp_inset_img()`)

The package also allows the optional use of packages designed for
transforming spatial data or modifying ggplot2 maps. These include:

-   [{smoothr}](https://strimas.com/smoothr/) (required to use the
    smooth_params argument)
-   [{ggfx}](https://ggfx.data-imaginist.com/) (required to use the
    shadow_params argument)

Many of the functions in {maplayer} were originally developed for the
[{overedge}](chttps://elipousson.github.io/overedge/) package or before
that for the
[{mapbaltimore}](https://elipousson.github.io/mapbaltimore/) package.
The {overedge} package has been split up into three smaller packages
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

### Add icons to a map

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

### Add a neatline to a map

`layer_neatline()` hides major grid lines and axis label by default. The
function is useful to draw a neatline around a map at a set aspect
ratio.

``` r
nc_map +
  layer_neatline(
    data = nc,
    asp = "6:4",
    color = "gray60", size = 2, linetype = "dashed"
  )
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

`layer_neatline()` can also be used to focus on a specific area of a map
with the option to apply a buffer as a distance or ratio of the diagonal
distance for the input data. The `label_axes` and `hide_grid` parameters
will not override a set ggplot theme.

``` r
nc_map +
  layer_neatline(
    data = nc[1, ],
    diag_ratio = 0.5,
    asp = 1,
    color = "black",
    label_axes = "--EN",
    hide_grid = FALSE,
    expand = FALSE
  )
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

### Add labels or numbers to a map

``` r
nc_map +
  layer_labelled(
    data = nc[c(10,20,30,40),],
    geom = "sf_label",
    label_col = "NAME"
  )
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

``` r
nc_map +
  layer_numbers(
    data = nc[c(10,20,30,40), ],
    aes(fill = NAME),
    num_style = "Alph",
    size = 3.5
    ) +
  guides(fill = "none")
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

### Create a frame around a feature

``` r
circle_nc <-
  ggplot(data = nc) +
  # layer_frame requires a data arg if neatline = TRUE
  layer_frame(data = nc, style = "circle") +
  # layer_location_data can inherit data
  layer_location_data()

circle_nc
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

### Create an inset map

``` r
nc_context <-
  layer_location_context(
      location = nc[25, ],
      context = nc,
      context_params = list(fill = "white", color = "gray80", alpha = 1),
      basemap = TRUE
    )

nc_context +
  layer_neatline(
    data = nc[25, ],
    dist = 10,
    unit = "mi",
    asp = 1
  ) +
  layer_inset(
    inset = nc_context,
    scale = 1.5
  )
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />
