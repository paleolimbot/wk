
library(tidyverse)
library(dbplyr)

db_file <- file.path("data-raw/proj.db")

# do this lazily because it takes 15 min or so to build PROJ
if (!file.exists(db_file)) {
  # download PROJ
  if (!dir.exists("data-raw/proj-9.1.0")) {
    source_url <- "https://download.osgeo.org/proj/proj-9.3.0.tar.gz"
    curl::curl_download(source_url, "data-raw/proj-source.tar.gz")
    untar("data-raw/proj-source.tar.gz", exdir = "data-raw")
  }

  # make sure the dir exists
  proj_dir <- list.files("data-raw", "^proj-[0-9.]+", include.dirs = TRUE, full.names = TRUE)
  stopifnot(dir.exists(proj_dir), length(proj_dir) == 1)

  # run configure and make (to make database) this takes a hot second
  withr::with_dir(proj_dir, {
    system("cmake .")
    system("cmake --build . --parallel 8")
  })

  # Copy database
  file.copy(file.path(proj_dir, "data", "proj.db"), db_file)

  # Clean up
  unlink(proj_dir, recursive = TRUE)
  unlink("data-raw/proj-source.tar.gz")
}

# extract some useful bits of the db into package data
db <- DBI::dbConnect(RSQLite::SQLite(), db_file)
src <- src_dbi(db)

# explore db tables
# src_tbls(src) %>%
#   set_names() %>%
#   map(~tbl(src, .x) %>% colnames())

# seems to be the view we're looking for
# arranges to make sure that we don't coerce integer to string
crs_view <- tbl(src, "crs_view") |>
  arrange(desc(code)) |>
  collect() |>
  arrange(auth_name, code)

defs <- crs_view %>%
  mutate(def = paste0(auth_name, ":", code)) %>%
  pull(def)

pb <- dplyr::progress_estimated(length(defs))
results_json <- map(defs, ~{
  pb$tick()$print()
  processx::run("projinfo", args = c("-o", "PROJJSON", "--single-line", "-q", .x))
})

pb <- dplyr::progress_estimated(length(defs))
results_wkt2 <- map(defs, ~{
  pb$tick()$print()
  processx::run("projinfo", args = c("-o", "WKT2:2019", "--single-line", "-q", .x))
})

# try to minify for disk size + validation
json <- map_chr(results_json, "stdout") %>%
  str_remove(fixed('"$schema":"https://proj.org/schemas/v0.5/projjson.schema.json",')) %>%
  map_chr(jsonlite::minify)

wkt2 <- map_chr(results_wkt2, "stdout") %>%
  str_trim()

# just to make sure we've got valid JSON
json_lst <- map(json, jsonlite::fromJSON)

crs_codes <- tibble(auth_name = crs_view$auth_name, code = crs_view$code)

wk_proj_crs_view <- as.data.frame(crs_view)
wk_proj_crs_json <- as.data.frame(bind_cols(crs_codes, tibble(projjson = json)))
wk_proj_crs_wkt2 <- as.data.frame(bind_cols(crs_codes, tibble(wkt2 = wkt2)))

usethis::use_data(wk_proj_crs_view, overwrite = TRUE)
usethis::use_data(wk_proj_crs_json, overwrite = TRUE)
usethis::use_data(wk_proj_crs_wkt2, overwrite = TRUE)
