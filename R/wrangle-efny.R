library(tidyverse)
library(scales)
library(fs)

# Because we haven't finalized our geocoding process for EFNY data and the
# geocoding is slow, we needed to take a few passes and ended up with multiple
# files that need to be stitch together first before creating the maps. Later we
# can come back and remove this script and update the SQL query once everything
# has been geocoded.

efny_declarations_all <- read_csv(path("data-private", "efny_declarations.csv"))

geocoded_files <- c(
  "efny_declarations_geocoded_archive1.csv",
  "efny_declarations_geocoded_archive2.csv",
  "efny_declarations_geocoded_archive3.csv"
)

geocoded_cases <- path("data-private", geocoded_files) %>% 
  map_dfr(~{
    read_csv(.x, col_types = cols_only(
      id = "d", lat = "d", lon = "d")
    )
  }) %>% 
  arrange(desc(lat)) %>% 
  distinct(id, .keep_all = TRUE) %>% 
  rename(lng = lon)

efny_declarations_analysis <- efny_declarations_all %>% 
  select(id, created_at) %>% 
  left_join(geocoded_cases, by = "id")

# No duplicates introduced
stopifnot(
  nrow(efny_declarations_analysis) == nrow(efny_declarations_all)
)

num_cases <- nrow(efny_declarations_analysis)
num_geocoded <- efny_declarations_analysis %>% filter(!is.na(lat)) %>% nrow()
pct_geocoded <- num_geocoded / num_cases

print(str_glue("{comma(num_geocoded)} of {comma(num_cases)} cases geocoded \\
               ({percent(pct_geocoded, 0.1)})"))
# 16,831 of 17,980 cases geocoded (93.6%)

efny_declarations_analysis %>% 
  write_csv(path("data-private", "efny_declarations_analysis.csv"), na = "")
