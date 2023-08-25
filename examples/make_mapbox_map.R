nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

make_mapbox_map(
  data = nc
)

make_mapbox_map(
  location = nc[2, ],
  style_url = "mapbox://styles/mapbox/outdoors-v12"
)

make_mapbox_map(
  location = nc[2, ],
  fg_layer = layer_location(data = nc[2, ], color = "white"),
  dist = 2,
  unit = "mi"
)

make_mapbox_map(
  location = nc[2, ],
  data = sf::st_sample(nc, 50),
  dist = 2,
  unit = "mi"
)
