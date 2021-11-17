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
#' @inheritParams ggplot2::geom_path
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


geom_textpath <- function(
  mapping = NULL, data = NULL, stat = "identity",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE,  ...,
  lineend = "butt", linejoin = "round", linemitre = 10
  )
{
  layer(geom = GeomTextpath, mapping = mapping, data = data, stat = stat,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
          na.rm     = na.rm,
          lineend   = lineend,
          linejoin  = linejoin,
          linemitre = linemitre,
          ...
        ))
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

GeomTextpath <- ggproto("GeomTextpath", Geom,
  required_aes = c("x", "y", "label"),

  # These aesthetics will all be available to the draw_panel function
  default_aes = aes(colour = "black", size = 3.88, hjust = 0.5, vjust = 0.5,
                    family = "", fontface = 1, lineheight = 1.2, alpha = 1,
                    linewidth = 0.5, linetype = 1),

  extra_params = c("na.rm"),

  # Do we want this draw_key?
  draw_key = draw_key_text,

  # The main draw_panel function is where we process our aesthetic data frame
  # into a tree of grobs for plotting.
  draw_panel = function(
    data, panel_params, coord,
    lineend = "butt", linejoin = "round", linemitre = 10
  ) {

    #---- type conversion, checks & warnings ---------------------------#

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

    #---- Set graphical parameters --------------------------#

    # browser()

    # Get first observation of each group
    first <- c(TRUE, data$group[-1] != data$group[-nrow(data)])

    text_gp <- gpar(
      col  = alpha(data$colour, data$alpha)[first],
      fontsize   = data$size[first] * .pt,
      fontface   = data$fontface[first],
      fontfamily = data$family[first],
      lineheight = data$lineheight[first]
    )

    path_gp <- gpar(
      col  = alpha(data$colour, data$alpha)[first],
      fill = alpha(data$colour, data$alpha)[first],
      lwd  = data$linewidth[first] * .pt,
      lty  = data$linetype[first],
      lineend   = lineend,
      linejoin  = linejoin,
      linemitre = linemitre
    )

    #---- Dispatch data to grob -----------------------------#

    textpathGrob(
      label = data$label[first],
      x = data$x,
      y = data$y,
      id = data$group,
      hjust = data$hjust[first],
      vjust = data$vjust[first],
      gp_text = text_gp,
      gp_path = path_gp,
      default.units = "npc"
    )
  }
)
