##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_textpath helpers                                                    ##
##                                                                           ##
##  Copyright (C) 2021 by Allan Cameron & Teun van den Brand                 ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

# Helpers -----------------------------------------------------------------

## Getting path points ----------------------------------------------------

#' Interpolate path at text locations
#'
#' This function aids in specifying the `x`, `y` and angle components of where
#' individual letters should be placed, of a single path-label pair.
#'
#' @param path A `data.frame` with the numeric columns `x`, `y`.
#' @param label A `data.frame` with measured text, such as one produced by the
#'   `measure_text()` function.
#' @param hjust A `numeric(1)` scalar specifying horizontal justification along
#'   the path.
#'
#' @return A `data.frame` with numerical values interpolated at the points where
#'   the letters in `label` argument should be placed, along with a `label`
#'   column containing individual glyphs of the string.
#' @noRd
#'
#' @details This is another helper function for the draw_panel function.
#' This is where
#' the text gets split into its component parts and assigned x, y and angle
#' components. This function also takes one group subset of the main panel data
#' frame at a time after .add_path_data() has been called, and returns a
#' modified data frame.
#'
#' The hjust is also applied here. Actually, although it's called hjust, this
#' parameter is really just analogous to hjust, and never gets passed to grid.
#' It determines how far along the path the string will be placed. The
#' individual letters all have an hjust of 0.5.
#'
#' @examples
#' xy <- data.frame(
#'   x =  1:10,
#'   y = (1:10)^2
#' )
#'
#' label <- measure_text("To be or not to be")[[1]]
#'
#' .get_path_points(xy, label)
.get_path_points <- function(
  path,
  label = "placeholder",
  hjust = 0.5,
  halign = "center",
  flip_inverted = FALSE
) {
  # We need a copy for a potential flip
  letters <- label

  # Calculate offsets and anchorpoints
  offset <- as_inch(attr(letters, "offset"))

  path$exceed <- .exceeds_curvature(path$x, path$y, d = offset)
  offset <- if(is.multichar(letters$glyph)) {
    .get_smooth_offset(path$x, path$y, d = offset)
  } else {
    .get_offset(path$x, path$y, d = offset)
  }
  anchor <- .anchor_points(offset, attr(letters, "metrics")$width,
                           hjust = hjust, halign = halign)

  # Offset text by anchorpoints
  xpos <- c("xmin", "xmid", "xmax")
  letters[, xpos] <- letters[, xpos] + anchor[letters$y_id]

  # Project text to curves

  letters <-  .project_text(letters, offset)

  # Consider flipping the text
  df <- .attempt_flip(path, label, letters$angle, hjust, halign, flip_inverted)
  if (!is.null(df)) {
    return(df)
  }

  # Interpolate whatever else is in `path` at text positions
  path$length <- .arclength_from_xy(path$x, path$y)

  protect_column <- c("x", "y", "angle", "length", "id", "left", "right")
  df <- as.list(path[setdiff(names(path), protect_column)])
  df <- approx_multiple(path$length, letters$base_length, df)
  df <- cbind(df, letters, id = path$id[1] %||% 1L)

  if(any(df$exceed != 0) & !is.multichar(label$glyph)) {

    warn(paste(
      "The text offset exceeds the curvature in one or more paths.",
      "This will result in\ndisplaced letters.",
      "Consider reducing the vjust or text size, or use the hjust\nparameter",
      "to move the string to a different point on the path."))
  }

  df[!is.na(df$angle), ]
}

#' Maybe flip text
#'
#' This function is just to capture the logic behind whether text ought to be
#' flipped.
#'
#' @inheritParams .get_path_points
#' @param angle A `numeric` vector with text angles in
#'
#' @return Either `NULL`, when text shouldn't be flipped, or a `data.frame` with
#'   flipped text if it should have been flipped.
#' @md
#' @noRd
#'
#' @examples
#' NULL
.attempt_flip <- function(
  path, label = "placeholder", angle = 0,
  hjust = 0, halign = "left", flip_inverted = FALSE
) {
  if (!flip_inverted) {
    return(NULL)
  }
  angle <- angle %% 360
  upside_down <- mean(angle > 100 & angle < 260) > 0.5
  if (!upside_down) {
    return(NULL)
  }
  # Invert path and hjust
  path  <- path[rev(seq_len(nrow(path))), ]
  attr(label, "offset") <- 0 - attr(label, "offset")

  if(is.numeric(hjust)) hjust <- 1 - hjust

  out <- .get_path_points(
    path, label, hjust, halign,
    flip_inverted = FALSE
  )
  # Invert length so path is trimmed correctly
  length <- path$length %||% .arclength_from_xy(path$x, path$y)
  out$length <- max(length) - out$length
  rights <-  max(length) - out$right
  out$right <- max(length) - out$left
  out$left <- rights


  out
}

#' Get anchor points
#'
#' This is a helper function that calculates for every offset what the anchor
#' position of text along the arc-length of an (offset) path should be.
#'
#' @param arc_length A `matrix` with `numeric` values, giving the arc-length of
#'   the original path in the first column, and a column for every offset-path.
#' @param text_width A `numeric` with the total width of the text.
#' @param hjust A `numeric` specifying horizontal justification of the text
#'   within the path.
#' @param halign A `character` specifying horizontal justification of the text
#'   among different lines in a multi-line text.
#' @return A `numeric` vector of length `ncol(arc_length)` with anchor points.
#' @md
#' @noRd
#'
#' @examples
#' arclength <- cbind(0:5, 0:5 * 2)
#' .anchor_points(arclength, 2.5, 0.5, "left")
.anchor_points <- function(
  offset, text_width, hjust = 0.5, halign = "center"
) {
  # Convert halign to a weight
  halign <- (match(halign, c("right", "center", "left")) - 1) / 2

  text_hjust <- if(is.numeric(hjust)) hjust[1] else 0.5

  if(is.character(hjust)) hjust <- interpret_hjust(hjust[1], offset, text_width)

  anchor <- hjust * offset$arc_length[nrow(offset$arc_length), 1]
  # Get left and right positions
  anchor <- anchor - (text_hjust + c(0, -1)) * text_width

  # Interpolate for offset paths
  anchor <- approx_multiple(offset$arc_length[, 1], anchor, offset$arc_length)

  # Weigh left and right anchors according to halign
  anchor[1, ] * halign + (1 - halign) * (anchor[2, ] - text_width)
}

#' Project text onto path
#'
#' This is a helper function that converts the position of letters from
#' arc-length space to Cartesian coordinates and calculates the appropriate
#' angle of the text.
#'
#' @param text A `list` with a row for every letter and at least the
#'   following columns: `xmin`, `xmid`, `xmax` for the positions of the glyph
#'   along the arc-length of a path and `y_id` for to which offset a letter
#'   belongs.
#' @param offset A `list` with at least 3 `matrix` elements describing the
#'   x, y positions and arc-lengths. Every row in these matrices correspond to
#'   a point on a path and every column holds an offsetted position, starting
#'   with no offset at the first column.
#'
#' @return A `data.frame` with the following columns: `label`, `length`,
#'   `angle`, `x` and `y` and `nrow(text)` rows.
#' @md
#' @noRd
#'
#' @examples
#' NULL
.project_text <- function(text, offset, xpos = c("xmin", "xmid", "xmax")) {
  arclength <- offset$arc_length
  index <- x <- unlist(text[, xpos], FALSE, FALSE)
  membr <- rep(text$y_id, 3)

  # Find indices along arc lengths
  split(index, membr) <- Map(
    findInterval,
    x    = split(index, membr),
    vec  = asplit(arclength[, sort(unique(membr)), drop = FALSE], MARGIN = 2),
    all.inside = TRUE
  )

  # Format indices for matrix-subsetting
  i0 <- cbind(index + 0, membr)
  i1 <- cbind(index + 1, membr)

  # Calculate weight of indices
  d  <- (x - arclength[i0]) / (arclength[i1] - arclength[i0])

  # Interpolate
  new_x <- offset$x[i0] * (1 - d) + offset$x[i1] * d
  new_y <- offset$y[i0] * (1 - d) + offset$y[i1] * d
  old_len <- arclength[i0[, 1], 1]
  lengs <- old_len * (1 - d) + arclength[i1[, 1], 1] * d

  # Restore dimensions
  # Column 1 comes from `xmin`, 2 from `xmid` and 3 from `xmax`
  dim(old_len) <- dim(new_x) <- dim(new_y) <- dim(lengs) <-
    c(nrow(text), length(xpos))

  # Calculate text angles
  dx <- new_x[, 3] - new_x[, 1]
  dy <- new_y[, 3] - new_y[, 1]
  angle <- atan2(dy, dx) * .rad2deg

  # Format output
  data_frame(
    label  = text$glyph,
    length = lengs[, 2],
    base_length = old_len[,1],
    angle  = angle,
    x = new_x[, 2],
    y = new_y[, 2],
    left  = lengs[, 1],
    right = lengs[, 3]
  )
}


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
#' @param cut_path A single logical TRUE or FALSE which if TRUE breaks the path
#'   into two sections, one on either side of the string and if FALSE leaves the
#'   path unbroken. The default value is NA, which will break the line if the
#'   string has a vjust of between 0 and 1
#' @param vjust_lim A `numeric` of length two setting the lower and upper limits
#'   of the `vjust` column in the `path` argument, which is used to decide
#'   whether a path should be trimmed or not when `cut_path = NA`.
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
.get_surrounding_lines <- function(path, letters, cut_path = NA,
                                   padding = 0.15, vjust = 0.5,
                                   vjust_lim = c(0, 1)) {
  padding <- as_inch(padding)
  if (is.unit(vjust)) {
    vjust <- rep_len(0.5, length(vjust))
  }

  trim <- vjust >= vjust_lim[1] & vjust <= vjust_lim[2]
  trim <- if (!is.na(cut_path)) rep(cut_path, length(trim)) else trim

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
      section = c(section, rep(c("pre", "post"), each = sum(trim)))
    )[order(c(path$length, ipol)), , drop = FALSE]

    # Filter empty sections (i.e., the part where the string is)
    path <- path[path$section != "", , drop = FALSE]
  }

  # Get first point of individual paths
  new_id <- paste0(path$id, "&", path$section)
  new_id <- discretise(new_id)
  start  <- c(TRUE, new_id[-1] != new_id[-length(new_id)])

  path$new_id <- new_id
  path$start  <- start


  return(path)
}

interpret_hjust <- function(hjust, offset, width) {

  x <- offset$x[, 1]
  y <- offset$y[, 1]
  path <- offset$arc_length[, 1]
  room <- 0.5 * width
  subset <- path > room & path < (max(path) - room)
  if (sum(subset) < 2) {
    subset <- rep(TRUE, length(path))
  }

  path <- path / max(path)

  switch(
    EXPR = hjust,
    auto = path[subset][which.min_curvature(x[subset], y[subset])],
    xmin = path[subset][which.min(x[subset])],
    xmax = path[subset][which.max(x[subset])],
    xmid = path[subset][which.min(abs(mean(x) - x[subset]))],
    ymin = path[subset][which.min(y[subset])],
    ymax = path[subset][which.max(y[subset])],
    ymid = path[subset][which.min(abs(mean(y) - y[subset]))],
    {
      warn(paste0("hjust value '", hjust, "' not recognised. ",
                  "Defaulting to hjust = 0.5"));
      return(0.5)
    }
  )
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
