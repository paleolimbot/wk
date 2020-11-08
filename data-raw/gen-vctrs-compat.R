
#  not automated yet, but probably should be since compat-vctrs.R is ~1,000 lines!

template_ptype2 <- '
#\' @method vec_ptype2.{ CLASS } { CLASS }
#\' @export
vec_ptype2.{ CLASS }.{ CLASS } <- function(x, y, ..., x_arg = "x", y_arg = "y") {{
  new_wk_{ CLASS }(crs = wk_crs_output(x, y))
}}

#\' @method vec_ptype2.{ CLASS } wk_wkb
#\' @export
vec_ptype2.{ CLASS }.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {{
  new_wk_PICK(crs = wk_crs_output(x, y))
}}

#\' @method vec_ptype2.{ CLASS } wk_wkt
#\' @export
vec_ptype2.{ CLASS }.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {{
  new_wk_PICK(crs = wk_crs_output(x, y))
}}

#\' @method vec_ptype2.{ CLASS } wk_wksxp
#\' @export
vec_ptype2.{ CLASS }.wk_wksxp <- function(x, y, ..., x_arg = "x", y_arg = "y") {{
  new_wk_PICK(crs = wk_crs_output(x, y))
}}

#\' @method vec_ptype2.{ CLASS } wk_xy
#\' @export
vec_ptype2.{ CLASS }.wk_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {{
  new_wk_PICK(crs = wk_crs_output(x, y))
}}

#\' @method vec_ptype2.{ CLASS } wk_xyz
#\' @export
vec_ptype2.{ CLASS }.wk_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {{
  new_wk_PICK(crs = wk_crs_output(x, y))
}}

#\' @method vec_ptype2.{ CLASS } wk_xym
#\' @export
vec_ptype2.{ CLASS }.wk_xym <- function(x, y, ..., x_arg = "x", y_arg = "y") {{
  new_wk_PICK(crs = wk_crs_output(x, y))
}}

#\' @method vec_ptype2.{ CLASS } wk_xyzm
#\' @export
vec_ptype2.{ CLASS }.wk_xyzm <- function(x, y, ..., x_arg = "x", y_arg = "y") {{
  new_wk_PICK(crs = wk_crs_output(x, y))
}}

#\' @method vec_ptype2.{ CLASS } wk_rct
#\' @export
vec_ptype2.{ CLASS }.wk_rct <- function(x, y, ..., x_arg = "x", y_arg = "y") {{
  new_wk_PICK(crs = wk_crs_output(x, y))
}}
'

template_cast <- '

#\' @method vec_cast.{ CLASS } wk_wkb
#\' @export
vec_cast.{ CLASS }.wk_wkb <- function(x, to, ...) {{
  as_{ gsub("wk_", "", CLASS) }(x, crs = wk_crs_output(x, to))
}}

#\' @method vec_cast.{ CLASS } wk_wkt
#\' @export
vec_cast.{ CLASS }.wk_wkt <- function(x, to, ...) {{
  as_{ gsub("wk_", "", CLASS) }(x, crs = wk_crs_output(x, to))
}}

#\' @method vec_cast.{ CLASS } wk_wksxp
#\' @export
vec_cast.{ CLASS }.wk_wksxp <- function(x, to, ...) {{
  as_{ gsub("wk_", "", CLASS) }(x, crs = wk_crs_output(x, to))
}}

#\' @method vec_cast.{ CLASS } wk_xy
#\' @export
vec_cast.{ CLASS }.wk_xy <- function(x, to, ...) {{
  as_{ gsub("wk_", "", CLASS) }(x, crs = wk_crs_output(x, to))
}}

#\' @method vec_cast.{ CLASS } wk_xyz
#\' @export
vec_cast.{ CLASS }.wk_xyz <- function(x, to, ...) {{
  as_{ gsub("wk_", "", CLASS) }(x, crs = wk_crs_output(x, to))
}}

#\' @method vec_cast.{ CLASS } wk_xym
#\' @export
vec_cast.{ CLASS }.wk_xym <- function(x, to, ...) {{
  as_{ gsub("wk_", "", CLASS) }(x, crs = wk_crs_output(x, to))
}}

#\' @method vec_cast.{ CLASS } wk_xyzm
#\' @export
vec_cast.{ CLASS }.wk_xyzm <- function(x, to, ...) {{
  as_{ gsub("wk_", "", CLASS) }(x, crs = wk_crs_output(x, to))
}}

#\' @method vec_cast.{ CLASS } wk_rct
#\' @export
vec_cast.{ CLASS }.wk_rct <- function(x, to, ...) {{
  as_{ gsub("wk_", "", CLASS) }(x, crs = wk_crs_output(x, to))
}}
'


with(
  list(CLASS = "wk_xy"),
  glue::glue(template_ptype2)
) %>%
  clipr::write_clip()

with(
  list(CLASS = "wk_xyz"),
  glue::glue(template_cast)
)
