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

# This function does the work of calculating the gradient of the path at each
# x, y value along its length, and the angle this implies that text should sit
# on the path (measured in degrees, not rads). It takes a group-subset of
# the layer data frame as input, so this function needs to be lapply-ed to the
# list formed by splitting the layer data frame by group. This has to be done
# _after_ transforming the data to co-ordinate space with coord$transform(),
# otherwise the angles will be wrong. This function could be moved into the
# body of draw_panel, but I have kept it as a separate non-exported function at
# the moment to keep the logic of this step separate.
#
# This function will be called after plot.new (or grid.newpage), so it will
# have access to the current device dimensions, etc. This is where we should do
# any calculations that take the aspect ratio into account to improve the angle
# of rotation for the letters.

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
  diff_rads <- diff(rads)
  diff_rads <- ifelse(diff_rads < - pi / 2, diff_rads + pi, diff_rads)
  diff_rads <- ifelse(diff_rads > pi / 2, diff_rads - pi, diff_rads)
  rads <- cumsum(c(rads[1], 0, diff_rads))

  # Now we can safely convert to degrees
  .data$angle <- rads * 180 / pi

  # Letters need to be spaced according to their distance along the path, so
  # we need a column to measure the distance of each point along the path
  .data$length <- c(0, cumsum(sqrt(diff(.data$x)^2 + diff(.data$y)^2)))

  # We also need to define curvature of the line at each point.
  # This is how much the angle changes per unit distance. We need to use
  # radians here. We need to know the curvature to increase or decrease
  # the spacing between characters when vjust is used, otherwise the spacing
  # will change

  diff_rads <- approx(seq_along(diff_rads), diff_rads,
                      seq(1, length(diff_rads), length.out = nrow(.data) - 1))$y

  curvature <- diff_rads/diff(.data$length)


  #curvature <- predict(loess(curvature ~ seq_along(curvature)))

  effective_length <- diff(.data$length) * (1 + (.data$vjust[1] - 0.5) * 0.04 *curvature)

  .data$adj_length <- c(0, cumsum(effective_length))

  .data
}


## Getting path points ----------------------------------------------------

# This is another helper function for the draw_panel function. This is where
# the text gets split into its component parts and assigned x, y and angle
# components. This function also takes one group subset of the main panel data
# frame at a time after .add_path_data() has been called, and returns a
# modified data frame.
#
# The total length of the textpath is currently implemented as the product of
# strwidth and text size multiplied by a "magic constant" that seems to look
# right on the plot (currently 0.5). Presumably there is a better way to do
# this.
#
# The hjust is also applied here. Actually, although it's called hjust, this
# parameter is really just analogous to hjust, and never gets passed to grid.
# It determines how far along the path the string will be placed. The
# individual letters all have an hjust of 0.5.

.get_path_points <- function(path)
{
  # The text needs some breathing space on either side if we are adding lines.
  # The easiest way to do this is to add spaces around the text string
  path$label <- paste0("  ", path$label, "  ")

  # Using the shape_string function from package "systemfonts" allows fast
  # and accurate calculation of letter spacing

  letters <- shape_string(strings    = path$label[1],
                          family     = path$family[1],
                          italic     = path$fontace[1] %in% c(3, 4),
                          bold       = path$fontface[1] %in% c(2, 4),
                          size       = path$size[1],
                          lineheight = path$lineheight[1])


  # We need to define a proportionality constant between mm and npc space
  k <- as.numeric(convertWidth(unit(1, "npc"), "mm"))

  # This gives us an accurate size for the letter placement in npc space
  letterwidths <- (letters$shape$x_offset + letters$shape$x_midpoint)/(k * 0.8)

  # This calculates the starting distance along the path where we place
  # the first letter
  start_dist <- path$hjust[1] * (max(path$adj_length) - max(letterwidths))

  # Now we just add on the letterwidths and we have the correct distances
  dist_points <- letterwidths + start_dist

  # We now need to interpolate all the numeric values along the path so we
  # get the appropriate values at each point. Non-numeric values should all
  # be identical, so these are just kept as-is


  df <- as.data.frame(lapply(path, function(i) {
    if(is.numeric(i))
      approx(x = path$adj_length, y = i, xout = dist_points, ties = mean)$y
    else
      rep(i[1], length(dist_points))
  }))


  # Now we assign each letter to its correct point on the path
  df$label <- letters$shape$glyph

  # This ensures that we don't try to return any invalid letters
  # (those letters that fall off the path on either side will have
  # NA angles)
  df[!is.na(df$angle), ]
}

## Getting surrounding lines -----------------------------------------------

# We probably want the option to draw the path itself, since this will be less
# work for the end-user. If the vjust is between 0 and 1 then the path will
# clash with the text, so we want to remove the segment where the text is.
# This function will get the correct segments in either case, but it needs
# the whole path data AND the calculated string data to do it.

## TODO: Do we want to add a parameter to switch the lines on and off,
##       inside geom_textpath(), or simply set a default linewidth of 0?
## RE: We could separate it into two geoms, one with a path by default and one
##     without. I think some graphics devices interpret 0-linewidth differently,
##     so the safer option would be to use `linetype = 0`, I think.

## TODO: Below, we're using `vjust` to determine where to cut the path if it
##       intersects text, but that doesn't take ascenders and descenders into
##       account.

## Can we rename this function to `.paths_bookends()`? I like the term bookend
## you used earlier in a comment!
.get_surrounding_lines <- function(path, letters) {

  # Early exit if text isn't exactly on path
  if (all(letters$vjust < 0) || all(letters$vjust > 1)) {
    path$section <- "all"
    return(path)
  }

  # Lengths of group runs (assumed to be sorted)
  # The `rle()` function handles NAs inelegantly,
  # but I'm assuming `group` cannot be NA.
  letter_lens <- rle(letters$group)$lengths
  curve_lens  <- rle(path$group)$lengths

  # Get locations where strings start and end
  starts <- {ends <- cumsum(letter_lens)} - letter_lens + 1
  mins <- letters$length[starts]
  maxs <- letters$length[ends]

  # Assign sections to before and after string
  path$section <- ""
  path$section[path$length < rep(mins, curve_lens)] <- "pre"
  path$section[path$length > rep(maxs, curve_lens)] <- "post"

  # Filter empty sections (i.e., the part where the string is)
  path[path$section != "", , drop = FALSE]
}
