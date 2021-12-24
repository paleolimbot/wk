
# registered in zzz.R
output_column.wk_vctr <- function(x, name) {
  out <- as.character(as_wkt(x))
  out[is.na(x)] <- NA_character_
  out
}

output_column.wk_rcrd <- function(x, name) {
  out <- as.character(as_wkt(x))
  out[is.na(x)] <- NA_character_
  out
}
