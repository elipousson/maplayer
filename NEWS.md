# maplayer (development version)

- Improve `labs_ext()` handling of blank values, drop `use_md` feature, and add new `source_note` argument.
- Rename `theme_sf_axes()` to `theme_sf_axis()` and fix handling of axis.text.x and axis.text.y theme elements. (2024-02-21)
- Update `map_icons` with [temaki v5.7.0](https://github.com/rapideditor/temaki/releases/tag/v5.7.0) (2024-02-21)

## 0.1.0.9003 (2023-08-25)

- Add `make_mapbox_map()` function and fix outstanding issues `make_layer_map()` helper.
- Add `{ggtext}` to Suggests (supporting new `use_md` parameter for `labs_ext()`)
- Add `{lwgeom}` to Suggests (avoiding error that popped up for `layer_count()`)
- Add `alpha` as a fixed aesthetic parameter for `layer_location()`
- Add `ggplot2::geom_sf_text` and `ggplot2::geom_sf_label` to re-exports
- Update `map_icons` with [temaki v5.4.0](https://github.com/rapideditor/temaki/releases/tag/v5.4.0)

# maplayer 0.1.0.9002 (2023-05-05)

- Add lower-level `make_layer_map()` helper for `make_location_map()`.
- Rename `make_basemap()` to `set_basemap()`

# maplayer 0.1.0.9001 (2023-04-10)

* Add `layer_arrow()`.
* Add default filetype ("gpkg") to `layer_location_data()`.
* Rename `null.ok` parameter to `allow_null`.
* Add `ggplot2::geom_sf()` to re-exports.
* Update `map_icons` with [maki v8.0.1](https://github.com/mapbox/maki/releases/tag/v8.0.1) and [temaki v5.3.0](https://github.com/rapideditor/temaki/releases/tag/v5.3.0).
* Add `{papersize}` and `{cliExtras}` to Imports.
* Remove `ggsave_ext()`, `map_ggsave_ext()`, and `ggsave_social()` from package functions but add to reexports from papersize.
* Drop `{purrr}` from Imports and adopt rlang equivalents from `standalone-purrr.R`.

# maplayer 0.1.0.9000 (2022-10-13)

- First version for package!
