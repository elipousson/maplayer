nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

plots <- lapply(
  dplyr::nest_by(nc, .by = NAME)[["data"]][1:4],
  function(x) {
    make_location_map(
      basemap = ggplot(),
      layer = layer_location(
        data = x,
        fill = "yellow",
        alpha = 0.5
      ),
      bg_layer = layer_location_data(
        data = nc,
        location = x,
        asp = 8.5 / 5.5,
        crop = FALSE
      ),
      neatline = layer_neatline(data = x, asp = 8.5 / 5.5),
      addon = labs_ext(caption = x$NAME)
    )
  }
)

make_atlas(
  plots = plots,
  page = "letter",
  nrow = 2,
  ncol = 1,
  save = FALSE
)
