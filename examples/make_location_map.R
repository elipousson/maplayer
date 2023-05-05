nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

make_location_map(
  location = nc
)

name <- "North Carolina"

make_location_map(
  location = nc,
  labs_ext_params = list(
    title = "Map of {name}"
  )
)

make_location_map(
  data = nc,
  location = nc[2, ],
  dist = 2,
  unit = "mi",
  crop = FALSE,
  labs_ext_params = list(
    title = "Map of {nc[2, ]$NAME} County"
  )
)

make_location_map(
  data = nc,
  location = nc[2, ],
  mapping = aes(fill = NAME),
  fg_layer = geom_sf_text(data = nc[2, ], aes(label = NAME)),
  addon = guides(fill = "none"),
  dist = 2,
  unit = "mi",
  crop = FALSE,
  neatline = TRUE,
  labs_ext_params = list(
    title = "Map of {nc[2, ]$NAME} and surrounding {name} counties"
  )
)
