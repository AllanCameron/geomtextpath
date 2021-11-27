##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_textpath main                                                       ##
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
#' @param label A `character(1)` scalar with a string to place.
#' @param gp An object of class `"gpar"`, typically the output from a call to
#'   the `grid::gpar()` function. Note that parameters related to fonts *must*
#'   be present. To be exact, the following parameters cannot be missing:
#'   `fontfamily`, `font`, `fontsize` and `lineheight`.
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
#' xy <- .add_path_data(xy)
#'
#' .get_path_points(xy)
.get_path_points <- function(
  path,
  label = "placeholder",
  gp = get.gpar(),
  hjust = 0.5, vjust = 0.5,
  halign = "center",
  flip_inverted = FALSE
) {

  ppi <- floor(convertUnit(unit(1, "in"), "pt", valueOnly = TRUE))

  path$length <- .arclength_from_xy(path$x, path$y)

  letters <- measure_text(label, gp = gp, vjust = vjust[1], halign = halign)

  letters$ymin <- letters$ymin - measure_text("x", gp = gp, vjust = 0.5)$ymin[1]

  y_pos <- unique(c(0, letters$ymin))

  offset <- .get_offset(path$x, path$y, d = y_pos)

  arclength <- offset$arc_length

  # Offset text x by anchorpoint
  xpos <- c("xmin", "xmid", "xmax")
  letters$yid <- match(letters$ymin, y_pos)

  anchor <- .anchor_points(arclength, attr(letters, "metrics")$width,
                           hjust = hjust, halign = halign)
  letters[, xpos] <- letters[, xpos] + anchor[letters$yid]

  letters <- .project_text(letters, offset)

  # Resolve inverted text
  if (flip_inverted) {
    upside_down <- letters$angle %% 360 > 100 & letters$angle %% 360 < 260
    if (mean(upside_down) > 0.5) {
      path <- path[rev(seq_len(nrow(path))), ]
      df <- .get_path_points(
        path, label, gp, hjust = 1 - hjust, halign = halign,
        flip_inverted = FALSE
      )
      return(df)
    }
  }

  # Format output
  df <- as.list(path[setdiff(names(path), c("x", "y", "angle", "length"))])
  is_num <- vapply(df, is.numeric, logical(1))
  df[is_num] <- lapply(df[is_num], function(i) {
    approx(x = path$length, y = i, xout = letters$length, ties = mean)$y
  })
  df[!is_num] <- lapply(lapply(df[!is_num], `[`, 1L),
                        rep, length.out = nrow(letters))

  df <- cbind(list_to_df(df), letters)
  df[!is.na(df$angle), ]
}

#' Wrapper for text measurement
#'
#' This wrap the `systemfonts::shape_string()` function to return positions for
#' every letter.
#'
#' @param label A `character(1)` of a label.
#' @param gp A `grid::gpar()` object.
#' @param ppi A `numeric(1)` for the resolution in points per inch.
#' @param vjust The justification of the text.
#'
#' @return A `data.frame` with the columns `glyph`, `ymin`, `xmin`, `xmid` and
#'   `xmax`.
#' @noRd
#'
#' @examples
#' measure_text("Hello there,\nGeneral Kenobi")
measure_text <- function(label, gp = gpar(), ppi = 72,
                         vjust = 0.5, hjust = 0, halign = "center") {

  halign <- match.arg(halign, c("center", "left", "right"))

  vjust[vjust == 1] <- 1 + .Machine$double.eps

  txt <- shape_string(
    strings    = label[1],
    family     = gp$fontfamily[1] %||% "",
    italic     = (gp$font[1]      %||% 1) %in% c(3, 4),
    bold       = (gp$font[1]      %||% 1) %in% c(2, 4),
    size       = gp$fontsize[1]   %||% 12,
    lineheight = gp$lineheight[1] %||% 1.2,
    tracking   = gp$tracking[1]   %||% 0,
    res = ppi,
    vjust = vjust,
    hjust = hjust,
    align = halign,
  )

  # Adjust metrics
  metrics <- txt$metrics
  metrics$width  <- metrics$width  / ppi
  metrics$height <- metrics$height / ppi

  # Adjust shape
  txt <- txt$shape
  txt$x_offset   <- txt$x_offset   / ppi
  txt$x_midpoint <- txt$x_midpoint / ppi

  # Format shape
  ans <- data_frame(
    glyph =  txt$glyph,
    ymin  =  txt$y_offset / ppi,
    xmin  =  txt$x_offset,
    xmid  = (txt$x_offset + txt$x_midpoint),
    xmax  = (txt$x_offset + txt$x_midpoint * 2)
  )
  ans <- ans[!(ans$glyph %in% c("\r", "\n", "\t")), , drop = FALSE]

  attr(ans, "metrics") <- metrics

  return(ans)
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
#'
#' @examples
#' arclength <- cbind(0:5, 0:5 * 2)
#' .anchor_points(arclength, 2.5, 0.5, "left")
.anchor_points <- function(
  arc_length, text_width, hjust = 0.5, halign = "center"
) {
  anchor <- hjust[1] * arc_length[nrow(arc_length), 1]
  # Get left and right positions
  anchor <- anchor - (hjust + c(0, -1)) * text_width

  # Interpolate for offset paths
  i <- findInterval(anchor, arc_length[, 1], all.inside = TRUE)
  d <- (anchor - arc_length[i, 1]) / (arc_length[i + 1, 1] - arc_length[i, 1])
  anchor <- arc_length[i, , drop = FALSE] * (1 - d) +
    arc_length[i + 1, , drop = FALSE] * d

  switch(
    halign,
    "left"   = anchor[1, ],
    "right"  = anchor[2, ] - text_width,
    "center" = (anchor[1, ] + anchor[2, ] - text_width) / 2
  )
}

#' Project text onto path
#'
#' This is a helper function that converts the position of letters from
#' arc-length space to Cartesian coordinates and calculates the appropriate
#' angle of the text.
#'
#' @param text A `data.frame` with a row for every letter and at least the
#'   following columns: `xmin`, `xmid`, `xmax` for the positions of the glyph
#'   along the arc-length of a path and `yid` for to which offset a letter
#'   belongs.
#' @param offset A `list` with at least 3 `matrix` elements describing the
#'   x, y positions and arc-lengths. Every row in these matrices correspond to
#'   a point on a path and every column holds an offsetted position, starting
#'   with no offset at the first column.
#'
#' @return A `data.frame` with the following columns: `label`, `length`,
#'   `angle`, `x` and `y` and `nrow(text)` rows.
#' @md
#'
#' @examples
#' NULL
.project_text <- function(text, offset) {
  arclength <- offset$arc_length
  index <- x <- unlist(text[, c("xmin", "xmid", "xmax")])
  membr <- rep(text$yid, 3)

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
  lengs <- arclength[i0[, 1], 1] * (1 - d) + arclength[i1[, 1], 1] * d

  # Restore dimensions
  # Column 1 comes from `xmin`, 2 from `xmid` and 3 from `xmax`
  dim(new_x) <- dim(new_y) <- dim(lengs) <- c(nrow(text), 3)

  # Calculate text angles
  dx <- new_x[, 3] - new_x[, 1]
  dy <- new_y[, 3] - new_y[, 1]
  angle <- atan2(dy, dx) * .rad2deg

  # Format output
  data_frame(
    label  = text$glyph,
    length = lengs[, 2],
    angle  = angle,
    x = new_x[, 2],
    y = new_y[, 2]
  )
}

## Getting surrounding lines -----------------------------------------------

## TODO: Sometimes when the device is really small or the letters huge, there
##       can be a letters data.frame that has 0 rows for a group. We should
##       defensively code something against this.

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
                                   breathing_room = 0.15, vjust = 0.5,
                                   vjust_lim = c(0, 1)) {

  trim <- vjust >= vjust_lim[1] & vjust <= vjust_lim[2]
  trim <- if (!is.na(cut_path)) rep(cut_path, length(trim)) else trim

  # Simplify if text isn't exactly on path
  if (!any(trim)) {
    path$section <- "all"
  } else {
    # Get locations where strings start and end
    ranges <- vapply(split(letters$length, letters$id), range,
                     numeric(2), USE.NAMES = FALSE)

    # Create breathing space around letters
    path_max <- vapply(split(path$length, path$id), max,
                       numeric(1), USE.NAMES = FALSE)
    trim <- rep_len(trim, length(path_max))

    mins <- pmax(0, ranges[1, ] - breathing_room)
    maxs <- pmin(path_max, ranges[2, ] + breathing_room)

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
    trim_x <- approx(path$length, path$x, ipol)$y
    trim_y <- approx(path$length, path$y, ipol)$y

    # Add trimming points to paths
    path <- data_frame(
      x  = c(path$x, trim_x),
      y  = c(path$y, trim_y),
      id = c(path$id, rep(which(trim), 2L)),
      section = c(section, rep(c("pre", "post"), each = sum(trim)))
    )[order(c(path$length, ipol)), , drop = FALSE]

    # Filter empty sections (i.e., the part where the string is)
    path <- path[path$section != "", , drop = FALSE]
  }

  if (nrow(path) > 0) {
    # Get first point of individual paths
    new_id <- paste0(path$id, "&", path$section)
    new_id <- discretise(new_id)
    start  <- c(TRUE, new_id[-1] != new_id[-length(new_id)])

    path$new_id <- new_id
    path$start  <- start
  } else {
    path$new_id <- integer(0)
    path$start  <- logical(0)
  }

  return(path)
}



