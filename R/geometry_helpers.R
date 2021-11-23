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

## Adding path data -------------------------------------------------------

#' Supplement path data
#'
#' This function supplements a single path given as x and y coordinates with
#' information about the shape of the curve.
#'
#' @param .data A `data.frame` with `x`, `y` and `vjust` numeric columns.
#'
#' @return A `data.frame` with additional columns `angle`, `length` and
#'   `adj_length`.
#' @noRd
#'
#' @details This function does the work of calculating the gradient of the path
#'   at each `x, y` value along its length, and the angle this implies that text
#'   should sit on the path (measured in degrees, not radians). It takes a
#'   group-subset of the layer data frame as input, so this function needs to
#'   be `lapply()`-ed to the list formed by splitting the layer data frame by
#'   group. This has to be done *after* transforming the data to co-ordinate
#'   space with `coord$transform()`, otherwise the angles will be wrong.
#'
#' @examples
#' xy <- data.frame(
#'   x =  1:10,
#'   y = (1:10)^2
#' )
#'
#' .add_path_data(xy)
.add_path_data <- function(.data)
{
  # Set default vjust if absent from data
  .data$vjust  <- .data$vjust %||% 0.5

  .data$angle  <- .path_angle_at_xy(.data$x, .data$y)

  .data$length <- .arclength_from_xy(.data$x, .data$y)

  offset <- .data$vjust - 0.5

  .data$adj_length  <- .length_adjust_by_curvature(.data$x, .data$y, offset)

  .data
}


## Getting path points ----------------------------------------------------

#' Interpolate path at text locations
#'
#' This function aids in specifying the `x`, `y` and angle components of where
#' individual letters should be placed, of a single path-label pair.
#'
#' @param path A `data.frame` with the numeric columns `x`, `y`, `angle`,
#'   `length` and `adj_length`.
#' @param label A `character(1)` scalar with a string to place.
#' @param gp An object of class `"gpar"`, typically the output from a call to
#'   the `grid::gpar()` function. Note that parameters related to fonts *must*
#'   be present. To be exact, the following parameters cannot be missing:
#'   `fontfamily`, `font`, `fontsize` and `lineheight`.
#' @param hjust A `numeric(1)` scalar specifying horizontal justification along
#'   the path.
#' @param flip_inverted If TRUE, any string where the majority of letters would
#'   be upside down along the path are inverted to improve legibility. The
#'   default is FALSE.
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
.get_path_points <- function(path, label = "placeholder",
                             gp = get.gpar(), hjust = 0.5,
                             flip_inverted = FALSE)
{
  # Get pixels per inch (72 is default screen resolution). For some reason text
  # renders weirdly if this is adapted to the device. For raster graphics,
  # one would typically use the following:
  # ppi <- dev.size("px")[1] / dev.size("in")[1]
  # But that gives the wrong spacing here.
  ppi <- 72

  # Using the shape_string function from package "systemfonts" allows fast
  # and accurate calculation of letter spacing

  letters <- shape_string(strings    = label[1],
                          family     = gp$fontfamily[1],
                          italic     = gp$font[1] %in% c(3, 4),
                          bold       = gp$font[1] %in% c(2, 4),
                          size       = gp$fontsize[1],
                          lineheight = gp$lineheight[1],
                          tracking   = gp$tracking[1] %||% 0,
                          res = ppi)

  letterwidths <- (letters$shape$x_offset + letters$shape$x_midpoint) / ppi

  # This calculates the starting distance along the path where we place
  # the first letter
  start_dist <- hjust[1] *
    (max(path$adj_length) - max(letterwidths + letters$shape$x_midpoint / ppi))

  # Now we just add on the letterwidths and we have the correct distances
  dist_points <- letterwidths + start_dist

  # We now need to interpolate all the numeric values along the path so we
  # get the appropriate values at each point. Non-numeric values should all
  # be identical, so these are just kept as-is

  df <- path[setdiff(names(path), c("x", "y", "angle"))]
  df <- list_to_df(lapply(df, function(i) {
    if(is.numeric(i))
      approx(x = path$adj_length, y = i, xout = dist_points, ties = mean)$y
    else
      rep(i[1], length(dist_points))
  }))

  # Instead of interpolating the angle from what we've calculated earlier and
  # what should  apply to the letter mid-points, we are re-calculating the angle
  # from the letter start and end points to get better angles for coarse paths
  letter_min <- letters$shape$x_offset / ppi + start_dist
  letter_max <- letter_min + 2 * letters$shape$x_midpoint / ppi

  # Interpolate x coordinates
  f    <- approxfun(x = path$adj_length, y = path$x)
  dx   <- f(letter_max) - f(letter_min)
  df$x <- f(dist_points)

  # Interpolate y coordinates
  f    <- approxfun(x = path$adj_length, y = path$y)
  dy   <- f(letter_max) - f(letter_min)
  df$y <- f(dist_points)

  # Recalculate angle
  df$angle <- atan2(dy, dx) * 180 / pi

  # Now we assign each letter to its correct point on the path
  df$label <- letters$shape$glyph

  # This ensures that we don't try to return any invalid letters
  # (those letters that fall off the path on either side will have
  # NA angles)
  df <- df[!is.na(df$angle), ]

  is_upside_down <- df$angle %% 360 > 100 & df$angle %% 360 < 260
  mostly_upside_down <- (sum(is_upside_down) / length(is_upside_down)) > 0.5

  if(mostly_upside_down & flip_inverted)
  {
    path <- path[rev(seq(nrow(path))),]
    path$vjust <- 1 - path$vjust_if_inverted
    path <- .add_path_data(path)
    df <- .get_path_points(path, label, gp, hjust = 1 - hjust)

  }
 df
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
                                   breathing_room = 0.15,
                                   vjust_lim = c(0, 1)) {

  path$trim <- (path$group_max_vjust >= vjust_lim[1] &
                path$group_min_vjust <= vjust_lim[2] ) |
               (path$group_max_vjust <= vjust_lim[2] &
                path$group_min_vjust >= vjust_lim[1])

  path$trim <- if (!is.na(cut_path)) rep(cut_path, nrow(path)) else path$trim

  # Simplify if text isn't exactly on path
  if (!any(path$trim)) {
    path$section <- "all"
  } else {
    trim <- path$trim[c(TRUE, path$id[-1] != path$id[-nrow(path)])]

    # Get locations where strings start and end
    letter_lens <- run_len(letters$id)
    starts <- {ends <- cumsum(letter_lens)} - letter_lens + 1
    mins <- letters$length[starts]
    maxs <- letters$length[ends]

    # Create breathing space around letters
    path_max <- vapply(split(path$length, path$id), max,
                       numeric(1), USE.NAMES = FALSE)

    mins <- pmax(0, mins - breathing_room)
    maxs <- pmin(path_max, maxs + breathing_room)

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



