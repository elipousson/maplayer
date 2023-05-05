get_repo_svg <- function(repo, branch = "main") {
  req <- httr2::request("https://api.github.com")
  req <- httr2::req_template(
    req,
    template = "repos/{repo}/git/trees/{branch}?recursive=1",
    repo = repo,
    branch = branch
  )

  resp <- httr2::req_perform(req) |>
    httr2::resp_body_json()

  data.frame(
    "path" = unlist(lapply(resp$tree, function(x) x$path))
  ) |>
    dplyr::filter(stringr::str_detect(path, "\\.svg$")) |>
    dplyr::transmute(
      repo = repo,
      name = stringr::str_extract(path, "(?<=/)[:graph:]+(?=.svg$)"),
      url = paste0("https://raw.githubusercontent.com/", repo, "/", branch, "/", path)
    )
}

maki <-
  get_repo_svg(repo = "mapbox/maki") |>
  dplyr::mutate(
    size = 15,
    style = ""
  )

temaki <-
  get_repo_svg(repo = "rapideditor/temaki") |>
  dplyr::mutate(
    size = 40,
    style = ""
  )

wu_icons <-
  get_repo_svg(repo = "manifestinteractive/weather-underground-icons", branch = "master") |>
  dplyr::mutate(
    size = 64,
    style = stringr::str_extract(name, "(?<=icons/).+(?=/svg)"),
    name = stringr::str_extract(name, "(?<=svg/).+")
  )

calcite <-
  get_repo_svg(repo = "Esri/calcite-point-symbols", branch = "master") |>
  dplyr::mutate(
    size = dplyr::case_when(
      stringr::str_detect(name, "13") ~ 13,
      stringr::str_detect(name, "17") ~ 17,
      stringr::str_detect(name, "21") ~ 21
    ),
    style = "",
    name = stringr::str_remove(name, "-[:digit:]+$")
  )

lane_icons <-
  get_repo_svg(repo = "openstreetmap/lane-icons", branch = "master") |>
  dplyr::mutate(
    size = 40,
    style = ""
  )

osm_map_icons <-
  get_repo_svg(repo = "openstreetmap/map-icons", branch = "master") |>
  dplyr::filter(stringr::str_detect(url, "/svg/")) |>
  dplyr::mutate(
    size = 40,
    style = ""
  )

nps_icons <-
  get_repo_svg(repo = "nationalparkservice/symbol-library", branch = "master") |>
  dplyr::mutate(
    size = dplyr::case_when(
      stringr::str_detect(name, "14") ~ 14,
      stringr::str_detect(name, "22") ~ 22,
      stringr::str_detect(name, "30") ~ 30
    ),
    style = dplyr::case_when(
      stringr::str_detect(url, "shielded.+") ~ "shielded-white",
      stringr::str_detect(name, "standalone.+white") ~ "standalone-white",
      stringr::str_detect(name, "standalone.+black") ~ "standalone-black"
    ),
    name = stringr::str_remove(name, "-white-[:digit:]+$"),
    name = stringr::str_remove(name, "-black-[:digit:]+$"),
    name = stringr::str_remove(name, "^shielded/"),
    name = stringr::str_remove(name, "^standalone/")
  )

map_icons <-
  dplyr::bind_rows(
    maki,
    temaki,
    wu_icons,
    lane_icons,
    osm_map_icons,
    calcite,
    nps_icons
  ) |>
  dplyr::relocate(
    repo,
    .after = dplyr::everything()
  ) |>
  dplyr::arrange(repo, name, style, dplyr::desc(size))

map_icons <- tibble::as_tibble(map_icons)

usethis::use_data(map_icons, overwrite = TRUE)
