##---------------------------------------------------------------------------##
##                                                                           ##
##  text_placement.R                                                         ##
##  Part of the geomtextpath R package                                       ##
##                                                                           ##
##  Copyright (C) 2021 by Allan Cameron & Teun van den Brand                 ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

# Main function -----------------------------------------------------------

# Interpolate path at text locations

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

  path$exceed <- exceeds_curvature(path$x, path$y, d = offset)
  offset <- if (is.multichar(letters$glyph)) {
    get_smooth_offset(path$x, path$y, d = offset)
  } else {
    get_offset(path$x, path$y, d = offset)
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

  if (!is.null(df)) return(df)

  # Interpolate whatever else is in `path` at text positions
  path$length <- arclength_from_xy(path$x, path$y)

  protect_column <- c("x", "y", "angle", "length", "id", "left", "right")
  df             <- as.list(path[setdiff(names(path), protect_column)])
  df             <- approx_multi(path$length, df, xout = letters$base_length)
  df             <- cbind(df, letters, id = path$id[1] %||% 1L)
  df$substring   <- label$substring %||% df$id
  df$xoffset     <- label$xoff
  df$yoffset     <- label$yoff

  if (any(df$exceed != 0) & !is.multichar(label$glyph)) {

    warn(paste(
      "The text offset exceeds the curvature in one or more paths.",
      "This will result in\ndisplaced letters.",
      "Consider reducing the vjust or text size, or use the hjust\nparameter",
      "to move the string to a different point on the path."))
  }

  df[!is.na(df$angle), ]
}

# Helpers -----------------------------------------------------------------

# Maybe flip text

attempt_flip <- function(
  path, label = "placeholder", angle = 0,
  hjust = 0, halign = "left", upright = FALSE
) {
  if (!upright)  return(NULL)

  angle       <- angle %% 360
  upside_down <- mean(angle > 100 & angle < 260) > 0.5

  if (!upside_down)  return(NULL)

  # Invert path and hjust
  path       <- path[rev(seq_len(nrow(path))), ]
  yids       <- unique(label$y_id)
  offset     <- as_inch(attr(label, "offset"))
  offlim     <- range(offset[yids])
  offnew     <- (offset - offlim[1]) - offlim[2]
  off_fix    <- as.numeric(any(yids == 1) && offnew[1] != 0)
  label$y_id <- label$y_id + off_fix

  attr(label, "offset") <- c(0, offnew[seq_along(offnew) >= (2 - off_fix)])

  if (is.numeric(hjust)) hjust <- 1 - hjust

  out <- place_text(path, label, hjust, halign, upright = FALSE)

  # Invert length so path is trimmed correctly
  length     <- path$length %||% arclength_from_xy(path$x, path$y)
  maxlen     <- max(length)
  out$length <- maxlen - out$length
  rights     <- maxlen - out$right
  out$right  <- maxlen - out$left
  out$left   <- rights

  out
}


anchor_points <- function(
  offset, text_width, hjust = 0.5, halign = "center"
) {
  # Convert halign to a weight
  halign <- (match(halign, c("right", "center", "left")) - 1) / 2

  text_hjust <- if (is.numeric(hjust)) hjust[1] else 0.5

  if (is.character(hjust)) {
    hjust <- interpret_hjust(hjust[1], offset, text_width)
  }

  anchor <- hjust * offset$arc_length[nrow(offset$arc_length), 1]
  # Get left and right positions
  anchor <- anchor - (text_hjust + c(0, -1)) * text_width

  # Interpolate for offset paths
  anchor <- approx_multi(offset$arc_length[, 1], offset$arc_length, anchor)

  # Weigh left and right anchors according to halign
  anchor[1, ] * halign + (1 - halign) * (anchor[2, ] - text_width)
}


interpret_hjust <- function(hjust, offset, width) {
  x          <- offset$x[, 1]
  y          <- offset$y[, 1]
  path       <- offset$arc_length[, 1]
  path_max   <- max(path)
  half_width <- 0.5 * width

  subset <- path > half_width & path < (path_max - half_width)
  if (sum(subset) < 2) subset[] <- TRUE

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
    end   = 1 + half_width / path_max, {
      warn(paste0("hjust value '", hjust, "' not recognised. ",
                  "Defaulting to hjust = 0.5"));
      return(0.5)
    }
  )
}

# Project text onto path

project_text <- function(text, offset, xpos = c("xmin", "xmid", "xmax")) {
  arclength  <- offset$arc_length
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
  new_x   <- offset$x[i0] * (1 - d) + offset$x[i1] * d
  new_y   <- offset$y[i0] * (1 - d) + offset$y[i1] * d
  old_len <- arclength[i0[, 1], 1]
  lengs   <- old_len * (1 - d) + arclength[i1[, 1], 1] * d

  # Restore dimensions
  # Column 1 comes from `xmin`, 2 from `xmid` and 3 from `xmax`
  dim(old_len) <- dim(new_x) <- dim(new_y) <- dim(lengs) <-
    c(nrow(text), length(xpos))

  # Calculate text angles
  dx    <- new_x[, 3] - new_x[, 1]
  dy    <- new_y[, 3] - new_y[, 1]
  angle <- atan2(dy, dx) * .rad2deg

  # Format output
  data_frame(
    label       = text$glyph,
    length      = lengs[, 2],
    base_length = old_len[, 1],
    angle       = angle,
    x           = new_x[, 2],
    y           = new_y[, 2],
    left        = lengs[, 1],
    right       = lengs[, 3]
  )
}

# Grob constructor --------------------------------------------------------

add_text_grob <- function(grob, text, gp) {
  sub       <- unlist(text$substring, FALSE, FALSE) %||% text$id
  text_lens <- run_len(sub)

  # Recycle graphical parameters to match lengths of letters
  gp <- recycle_gp(gp, rep, times = text_lens)

  if (is.list(text$xoffset)) {
    # This is a rich straight label
    xx    <- unlist(text$xoffset, FALSE, FALSE)
    yy    <- unlist(text$yoffset, FALSE, FALSE)
    nsub  <- lengths(text$xoffset)
    angle <- rep(text$angle, nsub) * .deg2rad
    x     <- xx * cos(angle) - yy * sin(angle) + rep(text$x, nsub)
    y     <- xx * sin(angle) + yy * cos(angle) + rep(text$y, nsub)
  } else {
    # This is a regular label
    x    <- text$x
    y    <- text$y
    nsub <- 1
  }

  # Write text grob
  grob <- addGrob(
    grob, textGrob(
      label = make_label(text$label),
      x = x, y = y, rot = rep(text$angle, nsub),
      vjust = 0.5, hjust = 0.5, gp = gp,
      default.units = "inches"
    )
  )
}
