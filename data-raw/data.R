
library(httr)
library(dplyr)
library(stringr)

get_repo_svg <- function(repo, branch = "main") {
  req <- GET(
    paste0("https://api.github.com/repos/", repo, "/git/trees/", branch, "?recursive=1")
  )

  data.frame(
    "path" = unlist(lapply(content(req)$tree, function(x) x$path))
  ) |>
    filter(str_detect(path, "\\.svg$")) |>
    transmute(
      repo = repo,
      name = str_extract(path, "(?<=/)[:graph:]+(?=.svg$)"),
      url = paste0("https://raw.githubusercontent.com/", repo, "/", branch, "/", path)
    )
}

maki <-
  get_repo_svg(repo = "mapbox/maki") |>
  mutate(
    size = 15,
    style = ""
  )

temaki <-
  get_repo_svg(repo = "ideditor/temaki") |>
  mutate(
    size = 40,
    style = ""
  )

wu_icons <-
  get_repo_svg(repo = "manifestinteractive/weather-underground-icons", branch = "master") |>
  mutate(
    size = 64,
    style = str_extract(name, "(?<=icons/).+(?=/svg)"),
    name = str_extract(name, "(?<=svg/).+")
  )

calcite <-
  get_repo_svg(repo = "Esri/calcite-point-symbols", branch = "master") |>
  mutate(
    size = case_when(
      str_detect(name, "13") ~ 13,
      str_detect(name, "17") ~ 17,
      str_detect(name, "21") ~ 21
    ),
    style = "",
    name = str_remove(name, "-[:digit:]+$")
  ) |>
  arrange(name, desc(size))

lane_icons <-
  get_repo_svg(repo = "openstreetmap/lane-icons", branch = "master") |>
  mutate(
    size = 40,
    style = ""
  )

osm_map_icons <-
  get_repo_svg(repo = "openstreetmap/map-icons", branch = "master") |>
  filter(str_detect(url, "/svg/")) |>
  mutate(
    size = 40,
    style = ""
  )

nps_icons <-
  get_repo_svg(repo = "nationalparkservice/symbol-library", branch = "master") |>
  mutate(
    size = case_when(
      str_detect(name, "14") ~ 14,
      str_detect(name, "22") ~ 17,
      str_detect(name, "30") ~ 30
    ),
    style = case_when(
      str_detect(url, "shielded.+") ~ "shielded-white",
      str_detect(name, "standalone.+white") ~ "standalone-white",
      str_detect(name, "standalone.+black") ~ "standalone-black"
    ),
    name = str_remove(name, "-white-[:digit:]+$"),
    name = str_remove(name, "-black-[:digit:]+$"),
    name = str_remove(name, "^shielded/"),
    name = str_remove(name, "^standalone/")
  ) |>
  arrange(name, desc(size))

map_icons <-
  bind_rows(
    maki,
    temaki,
    wu_icons,
    lane_icons,
    osm_map_icons,
    calcite,
    nps_icons
  ) |>
  relocate(
    repo,
    .after = everything()
  )

usethis::use_data(map_icons, overwrite = TRUE)
