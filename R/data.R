
#' Common CRS Representations
#'
#' These fixtures are calculated from PROJ version 9.1.0 and the database
#' built from its source. They are used internally to transform and inspect
#' coordinate reference systems.
#'
#' @examples
#' head(wk_proj_crs_view)
#' colnames(wk_proj_crs_json)
#'
"wk_proj_crs_view"

#' @rdname wk_proj_crs_view
"wk_proj_crs_json"

#' Create example geometry objects
#'
#' @param which An example name. Valid example names are
#'   - "nc"
#'   - "point", "linestring", "polygon", "multipoint",
#'     "multilinestring", "multipolygon", "geometrycollection"
#'   - One of the above with the "_z", "_m", or "_zm" suffix.
#' @inheritParams wk_crs
#' @inheritParams wk_is_geodesic
#'
#' @return A [wkt()] with the specified example.
#' @export
#'
#' @examples
#' wk_example("polygon")
#'
wk_example <- function(which = "nc",
                       crs = NA,
                       geodesic = FALSE) {
  all_examples <- wk::wk_example_wkt
  match.arg(which, names(all_examples))

  handleable <- all_examples[[which]]

  if (!identical(crs, NA)) {
    wk::wk_crs(handleable) <- crs
  }

  wk::wk_is_geodesic(handleable) <- geodesic
  handleable
}

#' @rdname wk_example
"wk_example_wkt"
