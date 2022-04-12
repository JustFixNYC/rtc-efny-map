library(tidyverse)
library(scales)
library(fs)

# Because we haven't finalized our geocoding process for OCA data and the
# geocoding is slow, we needed to take a few passes and ended up with multiple
# files that need to be stitch together first before creating the maps. Later we
# can come back and remove this script and update the SQL query once everything
# has been geocoded.

oca_cases_all <- read_csv(path("data-private", "oca_cases.csv"))
  
geocoded_files <- c(
  "oca_cases.csv", 
  "oca_cases_geocoded_archive1.csv",
  "oca_cases_geocoded_archive2.csv",
  "oca_cases_geocoded_archive3.csv"
)

geocoded_cases <- path("data-private", geocoded_files) %>% 
  map_dfr(~{
    read_csv(.x, col_types = cols_only(
      indexnumberid = "c", lat = "d", lon = "d")
    )
  }) %>% 
  arrange(desc(lat)) %>% 
  distinct(indexnumberid, .keep_all = TRUE) %>% 
  rename(lng = lon)

oca_cases_analysis <- oca_cases_all %>% 
  select(indexnumberid, court, fileddate, propertytype, status) %>% 
  left_join(geocoded_cases, by = "indexnumberid")

# No duplicates introduced
stopifnot(
  nrow(oca_cases_analysis) == nrow(oca_cases_all)
)

num_cases <- nrow(oca_cases_all)
num_geocoded <- oca_cases_analysis %>% filter(!is.na(lat)) %>% nrow()
pct_geocoded <- num_geocoded / num_cases

print(str_glue("{comma(num_geocoded)} of {comma(num_cases)} cases geocoded \\
               ({percent(pct_geocoded, 0.1)})"))
# 219,553 of 226,345 cases geocoded (97.0%)

oca_cases_analysis %>% 
  write_csv(path("data-private", "oca_cases_analysis.csv"), na = "")
