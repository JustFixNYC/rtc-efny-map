library(tidyverse) # general data manipulation
library(tidycensus) # download ACS data and shapes
library(viridisLite) # color palettes
library(sf) # spatial data
library(fs) # consistent file system operations
library(dotenv) # get env variables
library(leaflet) # interactive maps
# remotes::install_github("rstudio/leaflet.mapboxgl")
library(leaflet.mapboxgl) # nicer basemaps
library(scales) # number formatting
library(htmltools) # for map labels
library(htmlwidgets) # exporting maps

# Settings ----------------------------------------------------------------

# Edit ".env_sample" to set variables and save as ".env"
load_dot_env(".env")

census_api_key(Sys.getenv("ACS_API_KEY"))

options(tigris_use_cache = TRUE)

options(mapbox.accessToken = Sys.getenv("MAPBOX_TOKEN"))


# Constants ---------------------------------------------------------------

GEOGRAPHIES <- c(
  "zcta" = "zcta",
  "state-senate" = "state legislative district (upper chamber)",
  "state-assembly" = "state legislative district (lower chamber)"
)

ACS_VARS <- c(
  "pop_num" = "B01001_001",
  "pop_race_asian_num" = "B03002_006", 
  "pop_race_black_num" = "B03002_004",
  "pop_race_hisp_num" = "B03002_012",
  "pop_race_white_num" = "B03002_003",
  "hh_inc_med" = "B25119_001",
  "hh_inc_own_med" = "B25119_002",
  "hh_inc_rent_med" = "B25119_003"
)

RACE_LEVELS <- c("Asian", "Black", "Latinx", "White", "Other")

PAL_CONTINUOUS <- viridisLite::magma(10, begin = 0.1)

PAL_CATEGORIAL <- "Dark2" # from Rcolorbrewer

LAYER_NAMES <- c(
  "Active Eviction Cases", 
  "EFNY Hardship Declarations", 
  "Median Household Income, 2015-2019",
  "Predominant Race/Ethnicity, 2015-2019"
)

POPUP_GLUE <- str_c(
  "<b>{geo_name}</b><br>",
  "<br>Active Eviction Cases (OCA data as of 2022-04-05)",
  "<li><b>{comma(oca_cases, 1)}</b></li>",
  "<br>Hardship Declarations Submitted via EvictionFreeNY.org",
  "<li><b>{comma(efny_decs, 1)}</b></li>",
  "<br>Median Household Income, 2015-2019",
  "<li><b>{dollar(hh_inc_med, 1)}</b></li>",
  "<br>Race/Ethnicity, 2015-2019",
  "<li>Asian: {percent(pop_race_asian_pct, 0.1)}</li>",
  "<li>Black: {percent(pop_race_black_pct, 0.1)}</li>",
  "<li>Latinx: {percent(pop_race_hisp_pct, 0.1)}</li>",
  "<li>White: {percent(pop_race_white_pct, 0.1)}</li>",
  "<li>Other: {percent(pop_race_other_pct, 0.1)}</li>"
)

POPUP_OPTIONS <- popupOptions(
  # className = "leaflet-popup",
  # style = list(
  #   "font-size" = "14px"
  # )
)

LABEL_GLUE <- "<b>{geo_name}</b><br>Click for details"

LABEL_OPTIONS <- labelOptions(
  # direction = "top",
  # className = "leaflet-label"
)

HIGHLIGHT_OPTIONS = highlightOptions(
  weight = 2,
  color = "white",
  fillOpacity = 0.8,
  bringToFront = TRUE
)

# https://stackoverflow.com/a/52226825/7051239
TITLE_STYLE <- "
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"

# created from JF admin dashboard via "SQL/efny_query.sql"
# then geocoded with Geosupport & Nominatim (TODO: add info)
# then prepped for analysis in "R/wrangle-efny.R"
EFNY_POINTS <- path("data-private", "efny_declarations_analysis.csv") %>% 
  read_csv() %>% 
  # some records without coordinates, see "R/wrangle-efny.R"
  filter(!is.na(lng)) %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326)


# created from OCA Level-2 DB via "SQL/oca_cases.sql"
# then geocoded with Geosupport & Nominatim (TODO: add info)
# then prepped for analysis in "R/wrangle-oca.R"
OCA_POINTS <- path("data-private", "oca_cases_analysis.csv") %>% 
  read_csv() %>% 
  # some records without coordinates, see "R/wrangle-oca.R"
  filter(!is.na(lng)) %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326)


# downloaded via "R/download-shapefiles.R"
SHORELINE <- path("shapefiles", "State_Shoreline.shp") %>% 
  read_sf() %>% 
  st_union()


# ACS ---------------------------------------------------------------------

download_acs_with_shapes <- function(geography) {
  acs_raw <- get_acs(
    geography = geography,
    state = "36",
    variables = ACS_VARS,
    survey = "acs5",
    year = 2019,
    output = "wide",
    geometry = TRUE
  )
}


clean_acs_variables <- function(.data) {
  .data %>% 
    select(-ends_with("M")) %>% 
    rename(geoid = GEOID, geo_name = NAME) %>% 
    rename_with(~str_remove(., "E$"), ends_with("E")) %>% 
    mutate(
      pop_race_asian_pct = pop_race_asian_num / na_if(pop_num, 0), 
      pop_race_black_pct = pop_race_black_num / na_if(pop_num, 0),
      pop_race_hisp_pct  = pop_race_hisp_num  / na_if(pop_num, 0),
      pop_race_white_pct = pop_race_white_num / na_if(pop_num, 0),
      pop_race_other_num = pop_num - (pop_race_asian_num+ pop_race_black_num +
                                         pop_race_hisp_num +pop_race_white_num),
      pop_race_other_pct = pop_race_other_num/pop_num,
      pop_race_predom_pct = pmax(pop_race_asian_pct, pop_race_black_pct, pop_race_hisp_pct, pop_race_white_pct),
      pop_race_predom_name = case_when(
        pop_race_predom_pct == pop_race_asian_pct ~ "Asian",
        pop_race_predom_pct == pop_race_black_pct ~ "Black",
        pop_race_predom_pct == pop_race_hisp_pct ~ "Latinx",
        pop_race_predom_pct == pop_race_white_pct ~ "White",
        pop_race_predom_pct == pop_race_other_pct ~ "Other"
      ) %>% ordered(levels = RACE_LEVELS)
    ) %>% 
    select(-matches("pop_race_.*_num"))
}



# Count Points ------------------------------------------------------------


join_points_count  <- function(geo, points, col_name) {
  points_joined <- geo %>% 
    select(geoid, geometry) %>% 
    st_transform(2263) %>% 
    st_join(st_transform(points, 2263), join = st_intersects) %>% 
    st_drop_geometry()
  
  if ("indexnumberid" %in% names(points)) {
    # OCA
    points_joined <- points_joined %>% 
      distinct(indexnumberid, .keep_all = TRUE)
  } else if ("id" %in% names(points)) {
    # EFNY
    points_joined <- points_joined %>% 
      distinct(id, .keep_all = TRUE)
  }
  
  points_agg <- points_joined %>% 
    group_by(geoid) %>% 
    summarize({{ col_name }} := n()) %>% 
    full_join(
      select(st_drop_geometry(geo), geoid), 
      by = "geoid"
    ) %>% 
   mutate({{ col_name }} := replace_na(.data[[col_name]], 0))
  
  num_points_raw <- nrow(points)
  num_points_agg <- sum(points_agg[[col_name]])
  
  if (num_points_raw != num_points_agg) {
    warning(str_glue(
      "There are {num_points_raw} records in points data, but after joining \\
      with geographies there are {num_points_agg} points counted. \\
      Either some points did not match to geographies or duplicates were \\
      introduced by the join"), call. = FALSE)
  }
  
  geo %>% left_join(points_agg, by = "geoid")
  
}


# Geometries --------------------------------------------------------------

clip_to_shoreline <- function(shapes) {
  shapes %>% 
    # need consistent for spatial operation
    st_transform(2263) %>% 
    st_intersection(st_transform(SHORELINE, 2263)) %>% 
    # operation messes up the types and they need to be consistent for mapping
    mutate(geometry = st_cast(geometry, "MULTIPOLYGON")) %>% 
    st_transform(crs = st_crs(shapes))

}

simplify_shapes <- function(shapes, tolerance_ft) {
  shapes %>% 
    st_transform(2263) %>% 
    st_simplify(dTolerance = tolerance_ft) %>% 
    # operation messes up the types and they need to be consistent for mapping
    mutate(geometry = st_cast(geometry, "MULTIPOLYGON")) %>% 
    st_transform(crs = st_crs(shapes))
}


# Data --------------------------------------------------------------------

generate_data <- function(geo_name, simplify_ft) {
  
  GEOGRAPHIES[[geo_name]] %>%
    download_acs_with_shapes() %>%
    clean_acs_variables() %>%
    join_points_count(EFNY_POINTS, "efny_decs") %>% 
    join_points_count(OCA_POINTS, "oca_cases") %>% 
    clip_to_shoreline() %>%
    simplify_shapes(simplify_ft) %>% 
    st_transform(4326)
}

export_geojson <- function(.data, geo_name) {
  outfile <- path("data", str_glue("{geo_name}-map-data.geojson"))
  
  .data %>% 
    st_transform(4326) %>% 
    write_sf(outfile, delete_dsn = TRUE)
  
  invisible(.data)
}


# Mapping ------------------------------------------------------------------

add_polygons <- function(map, .data, col_name, pal, layer_name) {
  map %>% 
    addPolygons(
      data = .data,
      fillColor = pal(.data[[col_name]]),
      fillOpacity = 0.6,
      color = "white",
      weight = 0.3,
      opacity = 1,
      popup = ~str_glue(POPUP_GLUE),
      popupOptions = POPUP_OPTIONS,
      label = ~map(str_glue(LABEL_GLUE), HTML),
      labelOptions =LABEL_OPTIONS,
      highlightOptions = HIGHLIGHT_OPTIONS,
      group = layer_name
    )
}

add_choropleth_continuous <- function(map, .data, col_name, layer_name) {
  
  pal <- colorNumeric(PAL_CONTINUOUS, .data[[col_name]], na.color = "#bfbfbf")
  
  # The default legend has values low/top to high/bottom so need to reverse the palette
  pal_rev <- colorNumeric(PAL_CONTINUOUS, .data[[col_name]], na.color = "#bfbfbf", reverse=T)
  
  map %>% 
    add_polygons(.data, col_name, pal, layer_name) %>% 
    addLegend(
      "topleft", 
      pal = pal_rev, 
      values = .data[[col_name]],
      opacity = 1,
      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
      group = layer_name,
      title = layer_name
    )
  
}

add_choropleth_categorical <- function(map, .data, col_name, layer_name, levels) {
  
  pal <- colorFactor(PAL_CATEGORIAL, .data[[col_name]], na.color = "#bfbfbf", ordered = TRUE)
  
  map %>% 
    add_polygons(.data, col_name, pal, layer_name) %>% 
    addLegend(
      "topleft", 
      pal = pal, 
      # ensure that all are included even if not present, and in correct order
      values = ordered(levels, levels = levels),
      opacity = 1,
      group = layer_name,
      title = layer_name
    )
  
}

create_map <- function(.data, title) {
  
  tag.map.title <- tags$style(HTML(TITLE_STYLE))
  
  title_control <- tags$div(
    tag.map.title, HTML(title)
  )  
  
  leaflet() %>% 
    setView(lng = -73.801047, lat = 40.742274, zoom = 11) %>% 
    addMapboxGL(style = "mapbox://styles/mapbox/dark-v9") %>% 
    add_choropleth_continuous(.data, "oca_cases", "Active Eviction Cases") %>%
    add_choropleth_continuous(.data, "efny_decs", "EFNY Hardship Declarations") %>%
    add_choropleth_continuous(.data, "hh_inc_med", "Median Household Income, 2015-2019") %>%
    add_choropleth_categorical(.data, "pop_race_predom_name", "Predominant Race/Ethnicity, 2015-2019", RACE_LEVELS) %>% 
    addControl(title_control, position = "topleft", className="map-title") %>% 
    addLayersControl(
      overlayGroups = LAYER_NAMES,
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    hideGroup(LAYER_NAMES[-1])
}

export_map_html <- function(map, file_name, title) {
  htmlwidgets::saveWidget(
    widget = map, 
    file = path("docs", file_name), 
    selfcontained = FALSE,
    libdir = "libs",
    title = title
  )
  invisible(map)
}


# Senate Map --------------------------------------------------------------

senate_data <- generate_data("state-senate", 100) %>% 
  mutate(geo_name = str_remove(geo_name, "\\(.*")) %>% 
  export_geojson("state-senate")

create_map(senate_data, "RTC - Eviction Risk Map - State Senante") %>% 
  export_map_html("state-senate-map.html", "RTC Eviction Map State Senate")


# Assembly Map ------------------------------------------------------------

assembly_data <- generate_data("state-assembly", 100) %>% 
  mutate(geo_name = str_c("State ", str_remove(geo_name, "\\(.*"))) %>% 
  export_geojson("state-assembly")

create_map(assembly_data, "RTC - Eviction Risk Map - State Assembly") %>% 
  export_map_html("state-assembly-map.html", "RTC Eviction Map State Assembly")


# ZCTA Map ----------------------------------------------------------------

zcta_data <- generate_data("zcta", 100) %>% 
  mutate(geo_name = str_replace(geo_name, "ZCTA5", "Zip Code Tabulation Area")) %>% 
  export_geojson("zcta")

create_map(zcta_data, "RTC - Eviction Risk Map - Zip Code Tabulation Areas") %>% 
  export_map_html("zcta-map.html", "RTC Eviction Map State ZCTA")
