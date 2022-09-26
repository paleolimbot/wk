
#' Plot grid objects
#'
#' @inheritParams wk_plot
#' @param image A raster or nativeRaster to pass to [graphics::rasterImage()].
#'   use `NULL` to do a quick-and-dirty rescale of the data such that the low
#'   value is black and the high value is white.
#' @param oversample A scale on the number of pixels on the device to use for
#'   sampling estimation of large raster values. Use `Inf` to disable.
#' @param border Color to use for polygon borders. Use `NULL` for the default
#'   and `NA` to skip plotting borders.
#' @param interpolate Use `TRUE` to perform interpolation between color values.
#'
#' @return `x`, invisibly.
#' @export
#' @importFrom graphics plot
#'
#' @examples
#' plot(grd_rct(volcano))
#' plot(grd_xy(volcano))
#'
plot.wk_grd_xy <- function(x, ...) {
  plot(as_xy(x), ...)
  invisible(x)
}

#' @rdname plot.wk_grd_xy
#' @export
plot.wk_grd_rct <- function(x, ...,
                         image = NULL,
                         interpolate = FALSE,
                         oversample = 4,
                         border = NA,
                         asp = 1, bbox = NULL, xlab = "", ylab = "",
                         add = FALSE) {
  if (!add) {
    bbox <- unclass(bbox)
    bbox <- bbox %||% unclass(wk_bbox(x))
    xlim <- c(bbox$xmin, bbox$xmax)
    ylim <- c(bbox$ymin, bbox$ymax)

    graphics::plot(
      numeric(0),
      numeric(0),
      xlim = xlim,
      ylim = ylim,
      xlab = xlab,
      ylab = ylab,
      asp = asp
    )
  }

  # empty raster can skip plotting
  rct <- unclass(x$bbox)
  if (identical(rct$xmax - rct$xmin, -Inf) || identical(rct$ymax - rct$ymin, -Inf)) {
    return(invisible(x))
  }

  # as.raster() takes care of the default details here
  # call with native = TRUE, but realize this isn't implemented
  # everywhere (so we may get a regular raster back)
  if (is.null(image)) {

    # calculate an image based on a potential downsampling of the original
    usr <- graphics::par("usr")
    x_downsample_step <- grd_calculate_plot_step(x, oversample = oversample, usr = usr)

    # if we're so zoomed out that nothing should be plotted, we are done
    if (any(is.na(x_downsample_step))) {
      return(invisible(x))
    }

    x_downsample <- grd_crop(
      x,
      rct(usr[1], usr[3], usr[2], usr[4], crs = wk_crs(x)),
      step = x_downsample_step
    )

    # update bbox with cropped version and check for empty result
    rct <- unclass(x_downsample$bbox)
    if (identical(rct$xmax - rct$xmin, -Inf) || identical(rct$ymax - rct$ymin, -Inf)) {
      return(invisible(x))
    }

    image <- as.raster(x_downsample, native = TRUE)
  }

  if (!inherits(image, "nativeRaster")) {
    image <- as.raster(image, native = TRUE)
  }

  graphics::rasterImage(
    image,
    rct$xmin, rct$ymin, rct$xmax, rct$ymax,
    interpolate = interpolate
  )

  if (!identical(border, NA)) {
    if (is.null(border)) {
      border <- graphics::par("fg")
    }

    # simplify borders by drawing segments
    nx <- dim(x$data)[2]
    ny <- dim(x$data)[1]
    width <- rct$xmax - rct$xmin
    height <- rct$ymax - rct$ymin
    xs <- seq(rct$xmin, rct$xmax, by = width / nx)
    ys <- seq(rct$ymin, rct$ymax, by = height / ny)
    graphics::segments(xs, rct$ymin, xs, rct$ymax, col = border)
    graphics::segments(rct$xmin, ys, rct$xmax, ys, col = border)
  }

  invisible(x)
}

grd_calculate_plot_step <- function(grid, oversample = c(2, 2),
                                    usr = graphics::par("usr"),
                                    device = NULL) {
  # estimate resolution
  usr <- graphics::par("usr")
  usr_x <- usr[1:2]
  usr_y <- usr[3:4]

  device_x <- graphics::grconvertX(usr_x, to = "device")
  device_y <- graphics::grconvertY(usr_y, to = "device")

  # Use resolution of 1 at the device level, scale to usr coords.
  # this is sort of like pixels but not always; an oversample of
  # about 4 is needed for the operation to be invisible to the user
  scale_x <- abs(diff(device_x) / diff(usr_x))
  scale_y <- abs(diff(device_y) / diff(usr_y))
  resolution_x <- 1 / scale_x
  resolution_y <- 1 / scale_y

  # we need to know how many cells are going to be resolved by a crop
  # to see how many fewer of them we need to sample
  ranges <- grd_cell_range(grid, rct(usr_x[1], usr_y[1], usr_x[2], usr_y[2]))
  nx <- ranges$j["stop"] - ranges$j["start"]
  ny <- ranges$i["stop"] - ranges$i["start"]
  dx <- abs(diff(usr_x) / nx)
  dy <- abs(diff(usr_y) / ny)

  # calculate which step value is closest (rounding down, clamping to valid limits)
  step <- (c(resolution_y, resolution_x) / oversample) %/% c(dy, dx)
  step <- pmax(1L, step)

  # if the step is more than the number of pixels, it really shouldn't be
  # displayed at all
  if (any(step > c(ny, nx))) {
    return(c(NA_integer_, NA_integer_))
  }

  step
}

#' @export
#' @importFrom grDevices as.raster
as.raster.wk_grd_rct <- function(x, ..., i = NULL, j = NULL, native = NA) {
  # as.raster() works when values are [0..1]. We can emulate
  # this default by rescaling the image data if it's not already
  # a raster or nativeRaster.
  if (inherits(x$data, "nativeRaster") || grDevices::is.raster(x$data)) {
    grd_data_subset(x$data, i = i, j = j)
  } else if (prod(dim(x)) == 0) {
    as.raster(matrix(nrow = dim(x)[1], ncol = dim(x)[2]))
  } else if (length(dim(x)) == 2L || all(dim(x)[c(-1L, -2L)] == 1L)) {
    # try to interpret character() as hex colours, else
    # try to resolve x$data as a double() array
    if (is.character(x$data)) {
      return(as.raster(grd_data_subset(x$data, i = i, j = j)))
    }

    data <- grd_data_subset(x$data, i = i, j = j)
    storage.mode(data) <- "double"

    # we've checked that it's safe to drop the non-xy dimensions and
    # collected the data so that we know the axis order is R-native
    dim(data) <- dim(data)[1:2]

    range <- suppressWarnings(range(data, finite = TRUE))
    if (all(is.finite(range)) && (diff(range) > .Machine$double.eps)) {
      image <- (data - range[1]) / diff(range)
    } else if (all(is.finite(range))) {
      # constant value
      image <- data
      image[] <- 0.5
    } else {
      # all NA values
      image <- matrix(nrow = dim(data)[1], ncol = dim(data)[2])
    }

    as.raster(image)
  } else {
    stop(
      "Can't convert non-numeric or non-matrix grid to raster image",
      call. = FALSE
    )
  }
}
