library(fs)
library(purrr)

SHAPE_DIR <- path("shapefiles")

dir_create(SHAPE_DIR)

download.file(
  url = "https://gis.ny.gov/gisdata/fileserver/?DSID=927&file=NYS_Civil_Boundaries.shp.zip", 
  destfile = path(SHAPE_DIR, "NYS_Civil_Boundaries.shp.zip"),
  mode = "wb",
  quiet = TRUE
)

unzip(path(SHAPE_DIR, "NYS_Civil_Boundaries.shp.zip"), exdir = SHAPE_DIR)

dir_ls(SHAPE_DIR, glob = "*State_Shoreline*", invert = TRUE) %>% 
  walk(file_delete)
