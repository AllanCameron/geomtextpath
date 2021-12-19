
## Getting surrounding lines -----------------------------------------------

#' Trim text area from path
#'
#' This function splits a path when a string is predicted to intersect with
#' the path.
#'
#' @param path A `data.frame` with at least a numeric `length` column, an
#'   integer `id` column and `vjust` column. The `id` column must match that in
#'   the `letters` argument.
#' @param letters A `data.frame` with at least a numeric `length` column and
#'   integer `id` column. The `id` column must match that in the `path`
#'   argument.
#' @param gap A single logical TRUE or FALSE which if TRUE breaks the path
#'   into two sections, one on either side of the string and if FALSE leaves the
#'   path unbroken. The default value is NA, which will break the line if the
#'   string has a vjust of between 0 and 1
#' @param vjust_lim A `numeric` of length two setting the lower and upper limits
#'   of the `vjust` column in the `path` argument, which is used to decide
#'   whether a path should be trimmed or not when `gap = NA`.
#'
#' @details We probably want the option to draw the path itself, since this will
#'   be less work for the end-user. If the `vjust` is between 0 and 1 then the
#'   path will clash with the text, so we want to remove the segment where the
#'   text is. This function will get the correct segments in either case,
#'   but it needs the whole path data *and* the calculated string data to do it.
#'
#' @return The `path` data.frame filtered for clashing segments and including
#'   a `section` column indicated it was not clipped ("all"), before ("pre") or
#'   after ("post") clipping.
#' @noRd
#'
#' @examples
#' xy <- data.frame(
#'   x =  1:10,
#'   y = (1:10)^2,
#'   id = 1
#' )
#'
#' xy <- .add_path_data(xy)
#' glyphs <- .get_path_points(xy)
#' .get_surrounding_lines(xy, glyphs)
.get_surrounding_lines <- function(path, letters, gap = NA,
                                   padding = 0.15, vjust = 0.5,
                                   vjust_lim = c(0, 1)) {
  padding <- as_inch(padding)
  if (is.unit(vjust)) {
    vjust <- rep_len(0.5, length(vjust))
  }

  trim <- vjust >= vjust_lim[1] & vjust <= vjust_lim[2]
  trim <- if (!is.na(gap)) rep(gap, length(trim)) else trim

  # Simplify if text isn't exactly on path
  if (!any(trim)) {
    path$section <- "all"
  } else {
    path$length <- path$length %||% .arclength_from_xy(path$x, path$y, path$id)

    # Get locations where strings start and end
    lefts <- vapply(split(letters$left, letters$id), min,
                     numeric(1), USE.NAMES = FALSE)
    rights <- vapply(split(letters$right, letters$id), max,
                     numeric(1), USE.NAMES = FALSE)
    ranges <- rbind(lefts, rights)

    # Create breathing space around letters
    path_max <- vapply(split(path$length, path$id), max,
                       numeric(1), USE.NAMES = FALSE)
    trim <- rep_len(trim, length(path_max))

    mins <- pmax(0,        ranges[1, ] - padding)
    maxs <- pmin(path_max, ranges[2, ] + padding)

    # Consider path length as following one another to avoid a loop
    sumlen <- c(0, path_max[-length(path_max)])
    sumlen <- cumsum(sumlen + seq_along(path_max) - 1)
    mins <- mins + sumlen
    maxs <- maxs + sumlen
    path$length <- path$length + sumlen[path$id]

    # Assign sections based on trimming
    section <- character(nrow(path))
    section[path$length <= mins[path$id]] <- "pre"
    section[path$length >= maxs[path$id]] <- "post"
    section[!trim[path$id]] <- "all"

    # Interpolate trimming points
    ipol <- c(mins[trim], maxs[trim])
    trim_xy <- approx_multiple(path$length, ipol, path[c("x", "y")])

    # Add trimming points to paths
    path <- data_frame(
      x  = c(path$x, trim_xy$x),
      y  = c(path$y, trim_xy$y),
      id = c(path$id, rep(which(trim), 2L)),
      length = c(path$length, ipol),
      section = c(section, rep(c("pre", "post"), each = sum(trim)))
    )[order(c(path$length, ipol)), , drop = FALSE]

    # Filter empty sections (i.e., the part where the string is)
    path <- path[path$section != "", , drop = FALSE]

    # Filter empty paths
    group <- paste0(path$id, path$section)
    len <- ave(path$length, group, FUN = function(x) {diff(range(x))})
    path <- path[len > 1e-3, ]

    # Recategorise
    sect <- ave(path$section, path$id, FUN = function(x) length(unique(x)))
    path$section[sect == "1"] <- "all"
  }
  if (!nrow(path)) {
    path$new_id <- integer()
    path$start <- logical()
    return(path)
  }

  # Get first point of individual paths
  path$new_id <- group_id(path, c("id", "section"))
  path$start  <- c(TRUE, path$new_id[-1] != path$new_id[-length(path$new_id)])

  return(path)
}

#' Making a curved textbox
#'
#' @param path A `data.frame` containing `x` and `y` columns with numeric values.
#' @param text A `data.frame` containing `left` and `right` vectors with values
#'   along the arc-length of a path where text appears, and an `id` column.
#' @param label A `data.frame` as produced by `measure_text()`.
#' @param padding A `grid::unit()`.
#' @param radius A `grid::unit()`.
#'
#' @return A `data.frame` containing `x`, `y` and `id` columns for a closed
#'   polygon.
#' @noRd
#'
#' @examples
#' NULL
.curved_textbox <- function(
  path,
  text,
  label,
  padding = unit(0.25, "lines"),
  radius  = unit(0.15, "lines")
) {
  padding <- as_inch(padding)
  metrics <- attr(label, "metrics")
  height <- c(0, 1) * metrics$height + c(-1, 1) * padding

  if (nrow(text) > 1) {
    # Get min / mid / max height
    height <- c(0, 1) * metrics$height + c(-1, 1) * padding
    height <- c(height[1], sum(height) / 2, height[2])

    # Apply height to minimal offset
    offset <- as_inch(attr(label, "offset"))[unique(label$y_id)]
    offset <- min(offset) + metrics$x_adj
    offset <- c(0, offset + height)

    # Calculate offsets
    offset <- .get_offset(path$x, path$y, offset)

    # Set start / end at 0-offset, then translate to mid-height offset
    lims <- range(text$left, text$right)
    lims <- approx_multiple(offset$arc_length[, 1], lims, offset$arc_length[, 3])
    lims <- lims + c(-1, 1) * padding

    # Translate mid-height offset to min / max height offsets to get corners
    corners <- approx_multiple(
      offset$arc_length[, 3], lims,
      y = cbind(offset$x[, c(2, 4)], offset$y[, c(2, 4)])
    )

    # Check which points fall between corners
    keep  <- offset$arc_length[, 3] > lims[1] & offset$arc_length[, 3] < lims[2]
    nkeep <- sum(keep)

    # Add points in-between corners
    x <- c(corners[1, 1], offset$x[keep, 2],      corners[2, 1],
           corners[2, 2], rev(offset$x[keep, 4]), corners[1, 2])
    y <- c(corners[1, 3], offset$y[keep, 2],      corners[2, 3],
           corners[2, 4], rev(offset$y[keep, 4]), corners[1, 4])
  } else {
    # Simple rotation of a rectangle
    xx <- (metrics$width  * c(-0.5, 0.5) + c(-padding, padding))[c(1, 2, 2, 1)]
    yy <- (diff(height)   * c(-0.5, 0.5))[c(1, 1, 2, 2)]
    rot <- text$angle * .deg2rad
    x <- xx * cos(rot) - yy * sin(rot) + text$x
    y <- xx * sin(rot) + yy * cos(rot) + text$y
    nkeep <- 0
  }

  radius <- as_inch(radius)
  if (radius > 0.01) {
    if (radius > 0.5 * diff(range(height))) {
      radius  <- 0.5 * diff(range(height))
    }
    # Make closed polygon by inserting midpoint between start and end
    n <- length(x)
    x_start <- (x[1] + x[n]) / 2
    y_start <- (y[1] + y[n]) / 2
    x <- c(x_start, x, x_start)
    y <- c(y_start, y, y_start)

    # Round corners
    corners <- c(2, 3 + nkeep, 4 + nkeep, 5 + 2 * nkeep)
    xy <- .round_corners(x, y, radius, at = corners)
    x <- xy$x
    y <- xy$y
  }
  return(data_frame(x = x, y = y, id = text$id[1]))
}
