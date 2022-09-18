
# registered in zzz.R
output_column_wk <- function(x) {
  out <- as.character(as_wkt(x))
  out[is.na(x)] <- NA_character_
  out
}

output_column.wk_wkt <- function(x, name) {
  output_column_wk(x)
}

output_column.wk_wkb <- function(x, name) {
  output_column_wk(x)
}

output_column.wk_xy <- function(x, name) {
  output_column_wk(x)
}

output_column.wk_crc <- function(x, name) {
  output_column_wk(x)
}

output_column.wk_rct <- function(x, name) {
  output_column_wk(x)
}
