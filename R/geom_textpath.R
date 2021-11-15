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

# Constructor -------------------------------------------------------------

#' Add Curved Text Along Paths in \code{ggplot2}
#'
#' @description The existing text-based geom layers in ggplot2
#' (\code{geom_text} and \code{geom_label}) are ideal for the majority of plots,
#' since typically textual annotations are short, straight and in line with the
#' axes of the plot. However, there are some occasions when it is useful to have
#' text follow a curved path. This may be to create or recreate a specific
#' visual effect, or it may be to label a circular / polar plot in a more
#' "natural" way.
#'
#' There are limitations inherent in the plotting of text elements in
#' \code{ggplot} due to the way that the underlying \code{grid} graphics handles
#' text. A text string is dealt with as a zero-width object, and therefore the
#' rotation and spacing of the letters making up the string can only be dealt
#' with by treating each letter separately. Inevitably, this means
#' that curved text paths have to be calculated based on the size and aspect
#' ratio of the plotting device. Resizing the device after drawing a curved
#' text path will therefore cause artefacts of spacing and rotation in the text.
#'
#' It is important to realise that the letters are only rotated, and do not
#' undergo any change in shape. Thus, for example, large text appearing on
#' convex curves will not be deformed so that individual letters are narrower at
#' the bottom and wider at the top. Doing so would require reinterpreting the
#' letters as polygons.
#'
#' Another issue is that we may wish to use a short curved label on a much
#' longer path. Spacing the letters equally along the path would mean there is
#' too much space between the letters for the label to remain legible. A single
#' text string is therefore kept "together" according to the point size of the
#' text in \code{geom_textpath}. This then leaves the problem of where on the
#' path the text should be placed. This can be dealt with by the aesthetic
#' mapping \code{hjust}, which allows the user to place the labels
#' at the desired position along the path, including separate positions for
#' each label.
#'
#' A final point to note is that a path is usually a group-based geom (i.e.
#' a path typically comprises x, y points from two columns over several rows of
#' a data frame), whereas text labels can come from single rows in a data frame.
#' This means that if we have a data frame with an x column, a y column and a
#' grouping variable column, there can only be a single label for the group.
#' Typically, this will be the grouping variable itself (see the examples,
#' particularly those using the built-in \code{iris} data set.)
#'
#'
#' @inheritParams ggplot2::layer
#' @param ... other arguments passed on to \code{\link{layer}}. These are often
#' aesthetics, used to set an aesthetic to a fixed value, like \code{colour =
#' "red"} or \code{size = 3}. They may also be parameters to the paired
#'  geom/stat.
#' @param na.rm Removes missing points or labels from the text path.
#'
#' @section Aesthetics:
#' \code{geom_textpath()} understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \strong{\code{label}}
#'   \item \code{alpha}
#'   \item \code{colour}
#'   \item \code{family}
#'   \item \code{fontface}
#'   \item \code{group}
#'   \item \code{hjust}
#'   \item \code{size}
#'   \item \code{vjust}
#'   \item \code{hjust}
#'   \item \code{linetype}
#'   \item \code{linewidth}
#' }
#'
#' @export
#'
#' @examples
#'
#'# Plot text along an arbitrary path
#'
#' spiral <- data.frame(x = rev(sin(seq(0, 5*pi, length.out = 1000)) * 1000:1),
#'                      y = rev(cos(seq(0, 5*pi, length.out = 1000)) * 1000:1),
#'                      s = seq(1, 10, length.out = 1000),
#'                      z = paste("Like a circle in a spiral, like a",
#'                                "wheel within a wheel, never ending",
#'                                "or beginning on an ever spinning reel"))
#'
#' ggplot(spiral, aes(x, y, label = z)) +
#'   geom_textpath(size = 7.1, vjust = 2, linewidth = 0) +
#'   coord_equal(xlim = c(-1500, 1500), ylim = c(-1500, 1500))
#'
#'
#'# Produce labelled density lines:
#'
#' # By default the paths are broken to allow the names in-line
#'
#'  ggplot(iris, aes(x = Sepal.Length, color = Species)) +
#'   geom_textpath(aes(label = Species),
#'                 size = 8, stat = "density",
#'                 fontface = 2, hjust = 0.2)
#'
#' # If the vjust parameter moves the text above or below the line,
#' # the line is automatically filled in:
#'
#' ggplot(iris, aes(x = Sepal.Length, color = Species)) +
#'   geom_textpath(aes(label = Species), vjust = -0.1,
#'                 size = 8, stat = "density",
#'                 fontface = 2, hjust = 0.2)
#'
#'# label groups of points along their trend line:
#'
#' ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
#'   geom_point(alpha = 0.1) +
#'   geom_textpath(aes(label = Species, color = Species),
#'                 size = 8, stat = "smooth", linetype = 3,
#'                 fontface = 2, linewidth = 3) +
#'   scale_color_manual(values = c("forestgreen", "deepskyblue4", "tomato4")) +
#'   theme_bw()
#'
#' # Straight text paths in Cartesian Co-ordinates curve in Polar Co-ordinates
#'
#' df <- data.frame(x = 1:1000, y = 1, z = "This is a perfectly flat label")
#'
#' p <- ggplot(df, aes(x, y, label = z)) +
#'    geom_textpath(size = 6)
#'
#' p
#'
#' p + coord_polar()


geom_textpath <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE,  ...)
{
  layer(geom = GeomTextPath, mapping = mapping, data = data, stat = stat,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}

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
  # Simple calculation of the gradient
  grad <- diff(.data$y) / diff(.data$x)

  # The gradient vector needs to be the same length as input vectors. There are
  # various ways to do this, but interpolation is probably best:
  .data$grad <- approx(seq_along(grad), grad,
                  seq(1, length(grad), length.out = length(grad) + 1))$y

  # Gradient is converted to angle (in degrees) here:
  .data$angle <- atan(.data$grad) * 180 / pi

  # Since atan always outputs an angle between 0 and 180 degrees, we need to
  # subtract 180 degrees in cases where x is moving right to left
  .data$angle <- ifelse(sign(c(0, diff(.data$x))) < 0,
                        .data$angle - 180, .data$angle)

  # Letters need to be spaced according to their distance along the path, so
  # we need a column to measure the distance of each point along the path
  .data$length <- c(0, cumsum(sqrt(diff(.data$x)^2 + diff(.data$y)^2)))

  # We also need to define curvature of the line at each point.
  # This is how much the angle changes per unit distance. We need to use
  # radians here. We need to know the curvature to increase or decrease
  # the spacing between characters when vjust is used, otherwise the spacing
  # will change

  curvature <- diff(atan(.data$grad))/diff(.data$length)
  .data$curvature <- approx(seq_along(curvature), grad,
                 seq(1, length(curvature), length = length(curvature) + 1))$y

  # It will be useful to keep a record of the total length of the string we
  # wish to write. This will be used to normalise the width of the component
  # characters later on.
  .data$string_length <- sapply(.data$label, strwidth, units = "figure")

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

  # Split the string for this group, get its cumulative strwidth and multiply
  # it by the font size plus a "magic constant" to get it in line with the
  # equivalent spacing of geom_text

  ## TODO: 1) Incorporate curvature and vjust into line spacing
  ##       2) Try to work out if we can get rid of the magic constant
  ##          - what is its significance and why is it needed?
  ##       3) Consider more accurate alternatives to strwidth

  letters      <- strsplit(path$label[1], "")[[1]]
  letterwidths <- cumsum(c(0, strwidth(letters, font = path$fontface[1],
                                       units = "figure")))
  letterwidths <- path$size[1] * 0.3 * letterwidths
  letterwidths <- (head(letterwidths, -1) + tail(letterwidths, -1))/2

  # This calculates the starting distance along the path where we place
  # the first letter
  start_dist <- path$hjust[1] * (max(path$length) - max(letterwidths))

  # Now we just add on the letterwidths and we have the correct distances
  dist_points <- letterwidths + start_dist

  # We now need to interpolate all the numeric values along the path so we
  # get the appropriate values at each point. Non-numeric values should all
  # be identical, so these are just kept as-is
  df <- as.data.frame(lapply(path, function(i) {
    if(is.numeric(i))
      approx(x = path$length, y = i, xout = dist_points, ties = mean)$y
    else
      rep(i[1], length(dist_points))
    }))

  # Now we assign each letter to its correct point on the path
  df$label <- letters

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

.get_surrounding_lines <- function(path_data, letter_data)
{

  path_data$section <- character(nrow(path_data))
  path_data$section[path_data$length < head(letter_data$length, 1) ] <- "pre"
  path_data$section[path_data$length > tail(letter_data$length, 1) ] <- "post"

  if(letter_data$vjust[1] < 0 | letter_data$vjust[1] > 1) {
    path_data$section <- "all"
  }

  path_data[path_data$section != "", ]
}

# ggproto class -----------------------------------------------------------

#' The Geom object for a textpath
#'
#' This is the \code{ggproto} class that creates the textpath layer. It is not
#' intended to be used directly by the end user.
#'
#' @format NULL
#' @usage NULL
#' @export

GeomTextPath <- ggproto("GeomTextPath", Geom,
  required_aes = c("x", "y", "label"),

  # These aesthetics will all be available to the draw_panel function
  default_aes = aes(colour = "black", size = 3.88, hjust = 0.5, vjust = 0.5,
                             family = "", fontface = 1, lineheight = 1.2, alpha = 1,
                             linewidth = 2, linetype = 1),

  extra_params = c("na.rm"),

  # Do we want this draw_key?
  draw_key = draw_key_text,

  # The main draw_panel function is where we process our aesthetic data frame
  # into a tree of grobs for plotting.
  draw_panel = function(data, panel_params, coord) {

    #---- type conversion, checks & warnings ---------------------------#

    # strsplit doesn't like factors, so we need to ensure characters are used
    if(is.factor(data$label)) data$label <- as.character(data$label)

    # We want to be able to convert factors and strings into numbers to apply
    # the linetype aesthetic. It feels like thus is the wrong way to do it.
    # Presumably ggplot has a function to map strings/factors to scales
    if(is.character(data$linetype))
       data$linetype <- as.numeric(data$linetype)
    if(is.factor(data$linetype))
      data$linetype <- as.numeric(data$linetype)

    # We need to change groups to numeric to order them appropriately
    data$group <- as.numeric(factor(data$label))

    # Standard warning if row-wise data is passed instead of columnar groups.
    if (!anyDuplicated(data$group)) {
        ggplot2:::message_wrap("geom_textpath: Each group consists of only",
        "one observation. Do you need to adjust the group aesthetic?")
    }

    # If there is more than one text string associated with any of the groups,
    # we warn that only the first is used
    if(!all(sapply(split(data, data$group),
           function(x) all(x$label == x$label[1]))))
    {
         ggplot2:::message_wrap("geom_textpath: Multiple strings found in at",
         "least one group. Only the first will be used.")
    }

    #---- Data manipulation ---------------------------------#

    # Now we can sort the data by group
    data <- data[order(data$group), , drop = FALSE]

    # All our transformations occur after the coord transform:
    data <- coord$transform(data, panel_params)

    # Get gradients, angles and path lengths for each group
    data <- do.call(rbind, lapply(split(data, data$group), .add_path_data))

    # Get the actual text string positions & angles for each group
    data_points <- do.call(rbind, lapply(split(data, data$group), .get_path_points))

    # Calculate the bookending lines
    data_lines <- mapply(function(d1, d2) .get_surrounding_lines(d1, d2),
                          d1 = split(data, data$group),
                          d2 = split(data_points, data_points$group),
                         SIMPLIFY = FALSE)

    #---- Grob writing --------------------------------------#

    my_tree <- gTree()

    # Create the textgrobs
    for(i in seq(nrow(data_points)))
    {
      my_tree <- addGrob(my_tree, textGrob(
        label = data_points$label[i],
        x = data_points$x[i],
        y = data_points$y[i],
        vjust = data_points$vjust[i],
        hjust = 0.5,
        rot = data_points$angle[i],
        default.units = "native",
        gp = gpar(
          col = alpha(data_points$colour[i], data_points$alpha[i]),
          fontsize = data_points$size[i] * 2.85,
          fontface = data_points$fontface[i],
          fontfamily = data_points$fontfamily[i])))
    }


    # Create the linegrobs
    for(i in seq_along(data_lines))
    {
      d <- data_lines[[i]]
      d_list <- split(d, d$section)
      for(j in seq_along(d_list))
      {
        seg <- d_list[[j]]
        my_tree <- addGrob(my_tree, linesGrob(
          x = seg$x,
          y = seg$y,
          default.units = "native",
          gp = gpar(
            lty = seg$linetype,
            lwd = seg$linewidth,
            col = alpha(seg$colour[1], seg$alpha[1]))))
      }
    }

    return(my_tree)
  }
)
