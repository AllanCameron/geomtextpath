##---------------------------------------------------------------------------##
##                                                                           ##
##  text placement helpers                                                   ##
##                                                                           ##
##  Copyright (C) 2021 by Allan Cameron & Teun van den Brand                 ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

# Main function -----------------------------------------------------------

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
#' place_text(xy, label)
place_text <- function(
  path,
  label = "placeholder",
  hjust = 0.5,
  halign = "center",
  upright = FALSE
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
  anchor <- anchor_points(offset, attr(letters, "metrics")$width,
                          hjust = hjust, halign = halign)

  # Convert text from linear space to arclength space by offsetting the text
  # by the anchor points
  xpos <- c("xmin", "xmid", "xmax")
  letters[, xpos] <- letters[, xpos] + anchor[letters$y_id]

  # Convert text from arclength space to Cartesian space by projecting the
  # arclength positions onto the path
  letters <-  project_text(letters, offset)

  # Consider flipping the text
  df <- attempt_flip(path, label, letters$angle, hjust, halign, upright)
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

# Helpers -----------------------------------------------------------------

#' Maybe flip text
#'
#' This function is just to capture the logic behind whether text ought to be
#' flipped.
#'
#' @inheritParams place_text
#' @param angle A `numeric` vector with text angles in
#'
#' @return Either `NULL`, when text shouldn't be flipped, or a `data.frame` with
#'   flipped text if it should have been flipped.
#' @md
#' @noRd
#'
#' @examples
#' NULL
attempt_flip <- function(
  path, label = "placeholder", angle = 0,
  hjust = 0, halign = "left", upright = FALSE
) {
  if (!upright) {
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

  if (is.numeric(hjust)) hjust <- 1 - hjust

  out <- place_text(
    path, label, hjust, halign,
    upright = FALSE
  )
  # Invert length so path is trimmed correctly
  length <- path$length %||% .arclength_from_xy(path$x, path$y)
  maxlen <- max(length)
  out$length <- maxlen - out$length
  rights     <- maxlen - out$right
  out$right  <- maxlen - out$left
  out$left   <- rights

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
#' anchor_points(arclength, 2.5, 0.5, "left")
anchor_points <- function(
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

interpret_hjust <- function(hjust, offset, width) {

  x <- offset$x[, 1]
  y <- offset$y[, 1]
  path <- offset$arc_length[, 1]
  path_max <- max(path)
  half_width <- 0.5 * width

  subset <- path > half_width & path < (path_max - half_width)
  if (sum(subset) < 2) {
    subset <- rep(TRUE, length(path))
  }

  path <- path / path_max

  switch(
    EXPR = hjust,
    auto = path[subset][which.min_curvature(x[subset], y[subset])],
    xmin = path[subset][which.min(x[subset])],
    xmax = path[subset][which.max(x[subset])],
    xmid = path[subset][which.min(abs(mean(x) - x[subset]))],
    ymin = path[subset][which.min(y[subset])],
    ymax = path[subset][which.max(y[subset])],
    ymid = path[subset][which.min(abs(mean(y) - y[subset]))],
    start = 0 - half_width / path_max,
    end   = 1 + half_width / path_max,
    {
      warn(paste0("hjust value '", hjust, "' not recognised. ",
                  "Defaulting to hjust = 0.5"));
      return(0.5)
    }
  )
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
project_text <- function(text, offset, xpos = c("xmin", "xmid", "xmax")) {
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

# Grob constructor --------------------------------------------------------

.add_text_grob <- function(grob, text, gp) {
  text_lens <- run_len(text$id)

  # Recycle graphical parameters to match lengths of letters
  gp <- recycle_gp(gp, rep, times = text_lens)

  # Write text grob
  grob <- addGrob(
    grob, textGrob(
      label = make_label(text$label),
      x = text$x, y = text$y, rot = text$angle,
      vjust = 0.5, hjust = 0.5, gp = gp,
      default.units = "inches"
    )
  )
}
