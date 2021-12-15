
curl::curl_download(
  "https://github.com/fastfloat/fast_float/archive/refs/tags/v3.4.0.zip",
  "data-raw/fastfloat.zip"
)

unzip("data-raw/fastfloat.zip", exdir = "data-raw")
header_text <- withr::with_dir(
  "data-raw/fast_float-3.4.0",
  system("python3 script/amalgamate.py", intern = TRUE)
)

readr::write_lines(header_text, "src/fast_float.h")
