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
#' @param .data A `data.frame` with `x` and `y` numeric columns.
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
  # Gradient is found and converted to angle here. Since we use approx
  # to interpolate angles later, we can't have any sudden transitions
  # where angles "wrap around" from +180 to -180, otherwise we might
  # interpolate in this transition and get letters with an angle of
  # around 0. When combined with a vjust, this also makes the letters
  # jump out of alignment. This little algorithm makes sure the changes
  # in angle never wrap around.
  grad <- diff(.data$y) / diff(.data$x)
  rads <- atan(grad)
  if (length(rads) > 1) {
    diff_rads <- diff(rads)
    diff_rads <- ifelse(diff_rads < - pi / 2, diff_rads + pi, diff_rads)
    diff_rads <- ifelse(diff_rads > + pi / 2, diff_rads - pi, diff_rads)
    rads <- cumsum(c(rads[1], 0, diff_rads))
  } else {
    diff_rads <- c(0, 0)
    rads <- rep(rads, 2)
  }

  # Now we can safely convert to degrees
  .data$angle <- rads * 180 / pi

  # Letters need to be spaced according to their distance along the path, so
  # we need a column to measure the distance of each point along the path
  .data$length <- c(0, cumsum(sqrt(diff(.data$x)^2 + diff(.data$y)^2)))

  # We also need to define curvature of the line at each point.
  # This is how much the angle changes per unit distance. We need to use
  # radians here. We need to know the curvature to increase or decrease
  # the spacing between characters when vjust is used, otherwise the spacing
  # will be inconsistent across sections with different curvature

  diff_rads <- approx(seq_along(diff_rads), diff_rads,
                      seq(1, length(diff_rads), length.out = nrow(.data) - 1))$y

  curvature <- diff_rads/diff(.data$length)

  .data$vjust <- .data$vjust %||% 0.5 # Set default vjust if absent from data
  effective_length <- diff(.data$length) *
    (1 + ((head(.data$vjust, -1) + tail(.data$vjust, -1))/2 - 0.5) * curvature / 5)

  .data$adj_length <- c(0, cumsum(effective_length))

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
                             gp = get.gpar(), hjust = 0.5)
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
  df <- as.data.frame(lapply(df, function(i) {
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
  df[!is.na(df$angle), ]
}

## Getting surrounding lines -----------------------------------------------

## TODO: Do we want to add a parameter to switch the lines on and off,
##       inside geom_textpath(), or simply set a default linewidth of 0?
## RE: We could separate it into two geoms, one with a path by default and one
##     without. I think some graphics devices interpret 0-linewidth differently,
##     so the safer option would be to use `linetype = 0`, I think.

## TODO: Below, we're using `vjust` to determine where to cut the path if it
##       intersects text, but that doesn't take ascenders and descenders into
##       account.

## TODO: Sometimes when the device is really small or the letters huge, there
##       can be a letters data.frame that has 0 rows for a group. We should
##       defensively code something against this.

#' Trim text area from path
#'
#' This function splits a path when a string is predicted to intersect with
#' the path.
#'
#' @param path A `data.frame` with at least a numeric `length` column and
#'   integer `id` column. The `id` column must match that in the `letters`
#'   argument.
#' @param letters A `data.frame` with at least a numeric `length` column and
#'   integer `id` column. The `id` column must match that in the `path`
#'   argument.
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
.get_surrounding_lines <- function(path, letters) {

  # Simplify if text isn't exactly on path
  if (all(path$vjust < 0) || all(path$vjust > 1)) {
    path$section <- "all"
  } else {
    # Lengths of group runs (assumed to be sorted)
    # The `rle()` function handles NAs inelegantly,
    # but I'm assuming `id` cannot be NA.
    letter_lens <- rle(letters$id)$lengths
    curve_lens  <- rle(path$id)$lengths
    trim <- rep_len(!(path$vjust < 0 | path$vjust > 1), length(letter_lens))
    trim <- rep(trim, curve_lens)

    # Get locations where strings start and end
    starts <- {ends <- cumsum(letter_lens)} - letter_lens + 1
    mins <- letters$length[starts]
    maxs <- letters$length[ends]

    # Create breathing space around letters
    breathing_room <- 0.15
    path_max <- tapply(path$length, path$id, max)
    mins <- ifelse(mins < breathing_room, 0, mins - breathing_room)
    maxs <- ifelse(maxs > path_max - breathing_room, path_max,
                   maxs + breathing_room)

    # Assign sections to before and after string
    path$section <- ""
    path$section[path$length < rep(mins, curve_lens)] <- "pre"
    path$section[path$length > rep(maxs, curve_lens)] <- "post"
    path$section[!trim] <- "all"

    # Filter empty sections (i.e., the part where the string is)
    path <- path[path$section != "", , drop = FALSE]
  }

  if (nrow(path) > 1) {
    # Get first point of individual paths
    new_id <- paste0(path$id, "&", path$section)
    new_id <- match(new_id, unique(new_id))
    start  <- c(TRUE, new_id[-1] != new_id[-length(new_id)])

    path$new_id <- new_id
    path$start  <- start
  } else {
    path$new_id <- integer(0)
    path$start  <- logical(0)
  }

  return(path)
}



## Split linebreaks  -----------------------------------------------

#' Split strings with linebreaks into different groups
#'
#' This function prepares the data for plotting by splitting labels
#' at line breaks and giving each its own group
#'
#' @param data A `data.frame` with at least a factor or character column
#'   called "label", integer columns called "group" and "linetype", and
#'   numeric columns called "vjust" and "lineheight".
#'
#' @details The returned data is split into groups, one group for each
#'   segment of text such that none have line breaks. For strings that
#'   initially contained line breaks, they are broken up into different
#'   groups with different vjust values. The vjust values of each text line
#'   are centered around the originally specified vjust,
#'
#' @return A data frame containing the same column names and types as the
#'   original, but with newlines now treated as different groups.
#' @noRd
#'
#' @examples
#' xy <- data.frame(
#'   x =  1:10,
#'   y = (1:10)^2,
#'   group = 1,
#'   label = "This string \n has a line break",
#'   vjust = 0.5,
#'   linetype = 1,
#'   lineheight = 1.2
#' )
#'
#' .groupify_linebreaks(xy)
.groupify_linebreaks <- function(data)
{
    data$label <- as.character(data$label)
    line_breakers <- data[grepl("[\r\n]", data$label),]
    non_breakers <- data[!grepl("[\r\n]", data$label),]
    pieces <- strsplit(line_breakers$label, "[\r\n]+")
    line_breakers <- do.call(rbind, lapply(seq_along(pieces), function(i){
      n <- length(pieces[[i]])
      df <- line_breakers[rep(i, n),]
      df$label <- pieces[[i]]
      df$vjust <- (seq(n) - n/2 - 0.5) * df$lineheight[1] + df$vjust
      df$group <- rep(df$group[1] + seq(0, 1 - 1/n, 1/2),
                      length.out = nrow(df))
      line_type <- df$linetype[1]
      df$linetype <- 0
      df$linetype[which.max(nchar(df$label))] <- line_type
      df
    }))
    data <- rbind(line_breakers, non_breakers)

    data$group <- as.numeric(factor(data$group))

    data
}
