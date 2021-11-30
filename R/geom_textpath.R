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

#' Add Curved Text Along Paths in \pkg{ggplot2}
#'
#' @description The existing text-based geom layers in \pkg{ggplot2}
#' ([`geom_text()`][ggplot2::geom_text()] and
#' [`geom_label()`][ggplot2::geom_label()]) are ideal for the majority of plots,
#' since typically textual annotations are short, straight and in line with the
#' axes of the plot. However, there are some occasions when it is useful to have
#' text follow a curved path. This may be to create or recreate a specific
#' visual effect, or it may be to label a circular / polar plot in a more
#' "natural" way.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_path
#' @param ... other arguments passed on to [`layer()`][ggplot2::layer].
#'   These are often aesthetics, used to set an aesthetic to a fixed value,
#'   like `colour = "red"` or `size = 3`. They may also be parameters to the
#'   paired geom/stat.
#' @param na.rm If `FALSE` (default), missing points or labels are removed from
#'   the text path with a warning.
#' @param include_line A `logical(1)`, indicating whether a
#'   line should be plotted along with the text (`TRUE`, the default). If
#'   `FALSE`, any parameters or aesthetics relating to the drawing of the path
#'   in the layer will be ignored.
#' @param cut_path A `logical(1)` which if `TRUE` breaks the path
#'   into two sections, one on either side of the string. If `FALSE`, the
#'   path is plotted as a whole. The default, `NA`, will break the line if the
#'   string has a `vjust` of between 0 and 1.
#' @param flip_inverted A `logical(1)` which if `TRUE`, inverts any string where
#'   the majority of letters would be upside down along the path are inverted to
#'   improve legibility. The default is `FALSE`, which leaves letters as-is.
#' @param halign A `character(1)` describing how multi-line labels should
#'   be justified. Can either be `"left"` (default), `"center"` or `"right"`.
#' @param offset A [`unit()`][grid::unit()] of length 1 to determine the offset
#'   of the text from the path. If not `NULL`, this overrules the `vjust`
#'   setting.
#'
#' @details
#' There are limitations inherent in the plotting of text elements in
#' \pkg{ggplot2} due to the way that the underlying \pkg{grid} graphics handles
#' text. A text string is dealt with as a zero-width object, and therefore the
#' rotation and spacing of the letters making up the string can only be dealt
#' with by treating each letter separately.
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
#' text in `geom_textpath()`. This then leaves the problem of where on the
#' path the text should be placed. This can be dealt with by the aesthetic
#' mapping `hjust`, which allows the user to place the labels
#' at the desired position along the path, including separate positions for
#' each label.
#'
#' A final point to note is that a path is usually a group-based geom (i.e.
#' a path typically comprises x, y points from two columns over several rows of
#' a data frame), whereas text labels can come from single rows in a data frame.
#' This means that if we have a data frame with an x column, a y column and a
#' grouping variable column, there can only be a single label for the group.
#' Typically, this will be the grouping variable itself (see the examples,
#' particularly those using the built-in `iris` data set.)
#'
#' @section Aesthetics:
#' The `spacing` aesthetic allows fine control of spacing of text,
#' which is called 'tracking' in typography. The default is 0 and units are
#' measured in 1/1000 em. Numbers greater than zero increase the spacing,
#' whereas negative numbers decrease the spacing.
#' `geom_textpath()` understands the following aesthetics (required
#' aesthetics are in bold):
#' \itemize{
#'   \item \strong{`x`}
#'   \item \strong{`y`}
#'   \item \strong{`label`}
#'   \item `alpha`
#'   \item `colour`
#'   \item `family`
#'   \item `fontface`
#'   \item `group`
#'   \item `hjust`
#'   \item `size`
#'   \item `vjust`
#'   \item `linetype`
#'   \item `linewidth`
#'   \item `linecolour`
#'   \item `spacing`
#' }
#'
#' @export
#' @md
#'
#' @examples
#'# Plot text along an arbitrary path
#'  t <- seq(-1, 5, length.out = 1000) * pi
#'  spiral <- data.frame(
#'    x = rev(sin(t) * 1000:1),
#'    y = rev(cos(t) * 1000:1),
#'    s = seq(1, 10, length.out = 100),
#'    text = paste(
#'      "Like a circle in a spiral, like a wheel within a wheel,",
#'      "never ending or beginning on an ever spinning reel"
#'   )
#' )
#'
#'  ggplot(spiral, aes(x, y, label = text)) +
#'    geom_textpath(size = 7, vjust = 2, linewidth = 0) +
#'    coord_equal(xlim = c(-1500, 1500), ylim = c(-1500, 1500))
#'
#'# Produce labelled density lines:
#'
#' # By default the paths are broken to allow the names in-line
#'
#'  ggplot(iris, aes(x = Sepal.Length, colour = Species)) +
#'    geom_textpath(aes(label = Species), stat = "density",
#'                  size = 6, fontface = 2, hjust = 0.2, vjust = 0.3)
#'
#' # If the vjust parameter moves the text above or below the line,
#' # the line is automatically filled in:
#'
#'  ggplot(iris, aes(x = Sepal.Length, colour = Species)) +
#'    geom_textpath(aes(label = Species), stat = "density",
#'                  size = 6, fontface = 2, hjust = 0.2, vjust = -0.2)
#'
#'# Correction of angles across different aspect ratios:
#'
#' # The angle of the text continues to follow the path even if the
#' # aspect ratio of the plot changes, for example, during faceting.
#' # Compare faceting horizontally:
#'
#'  p <- ggplot(iris, aes(x = Sepal.Length, colour = Species)) +
#'         geom_textpath(aes(label = Species), stat = "density",
#'                       size = 6, fontface = 2, hjust = 0.1, vjust = -0.2) +
#'         scale_y_continuous(limits = c(0, 1.5))
#'
#'  p + facet_grid(.~Species)
#'
#' # and faceting vertically:
#'
#'  p + facet_grid(Species~.)
#'
#'# label groups of points along their trend line:
#'
#'  ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
#'    geom_point(alpha = 0.1) +
#'    geom_textpath(aes(label = Species, colour = Species),
#'                  stat = "smooth", method = "loess", formula = y ~ x,
#'                  size = 7, linetype = 3, fontface = 2, linewidth = 1) +
#'    scale_colour_manual(values = c("forestgreen", "deepskyblue4", "tomato4")) +
#'    theme_bw()
#'
#' # Straight text paths in Cartesian Co-ordinates curve in Polar Co-ordinates
#'
#'  df <- data.frame(x = 1:1000, y = 1, text = "This is a perfectly flat label")
#'
#'  p <- ggplot(df, aes(x, y, label = text)) +
#'    geom_textpath(size = 6)
#'  p
#'
#' p + coord_polar(start = pi)


geom_textpath <- function(
  mapping = NULL, data = NULL, stat = "identity",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE,  ...,
  lineend = "butt", linejoin = "round", linemitre = 10,
  include_line = TRUE, cut_path = NA, flip_inverted = FALSE,
  halign = "left", offset = NULL
  )
{
  layer(geom = GeomTextpath, mapping = mapping, data = data, stat = stat,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
          na.rm         = na.rm,
          lineend       = lineend,
          linejoin      = linejoin,
          linemitre     = linemitre,
          include_line  = include_line,
          cut_path      = cut_path,
          flip_inverted = flip_inverted,
          halign        = halign,
          offset        = offset,
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
                    linewidth = 0.5, linetype = 1, spacing = 0,
                    linecolour = "_copy_text_colour_", angle = 0),

  extra_params = c("na.rm", "include_line"),

  setup_data = function(data, params) {
    if (isFALSE(params$include_line)) {
      data$linetype <- 0
    }
    if (all(data$group == -1) && !is.null(data$label)) {
      data$group <- discretise(data$label)
    }
    data
  },

  # Do we want this draw_key?
  draw_key = draw_key_text,

  # The main draw_panel function is where we process our aesthetic data frame
  # into a tree of grobs for plotting.
  draw_panel = function(
    data, panel_params, coord,
    lineend = "butt", linejoin = "round", linemitre = 10,
    cut_path = NA, flip_inverted = FALSE, halign = "left",
    offset = NULL
  ) {

    #---- type conversion, checks & warnings ---------------------------#

    # We want to be able to convert factors and strings into numbers to apply
    # the linetype aesthetic. It feels like thus is the wrong way to do it.
    # Presumably ggplot has a function to map strings/factors to scales
    if(is.character(data$linetype))
       data$linetype <- as.numeric(data$linetype)
    if(is.factor(data$linetype))
      data$linetype <- as.numeric(data$linetype)

    copy_colour <- data$linecolour == "_copy_text_colour_"
    data$linecolour[copy_colour] <- data$colour[copy_colour]

    # We need to change groups to numeric to order them appropriately
    data$group <- discretise(data$group)

    # If there is more than one text string associated with any of the groups,
    # we warn that only the first is used
    if(!all(sapply(split(data, data$group),
           function(x) all(x$label == x$label[1]))))
    {
         ggplot2:::message_wrap("geom_textpath: Multiple strings found in at ",
         "least one group. Only the first will be used.")
    }

    #---- Data manipulation ---------------------------------#

    # Now we can sort the data by group
    data <- data[order(data$group), , drop = FALSE]

    # All our transformations occur after the coord transform:
    data <- coord_munch(coord, data, panel_params)

    #---- Set graphical parameters --------------------------#

    # Get first observation of each group
    first <- c(TRUE, data$group[-1] != data$group[-nrow(data)])

    text_gp <- gpar(
      col  = alpha(data$colour, data$alpha)[first],
      fontsize   = data$size[first] * .pt,
      fontface   = data$fontface[first],
      fontfamily = data$family[first],
      lineheight = data$lineheight[first],
      tracking   = data$spacing[first]
    )

    if (all(data$linetype == 0)) {
      path_gp <- gpar(lty = 0)
    } else {
      path_gp <- gpar(
        col  = alpha(data$linecolour, data$alpha)[first],
        fill = alpha(data$linecolour, data$alpha)[first],
        lwd  = data$linewidth[first] * .pt,
        lty  = data$linetype[first],
        lineend   = lineend,
        linejoin  = linejoin,
        linemitre = linemitre
      )
    }

    #---- Dispatch data to grob -----------------------------#

    textpathGrob(
      label = data$label[first],
      x = data$x,
      y = data$y,
      id = data$group,
      hjust  = data$hjust[first],
      vjust  = offset %||% data$vjust,
      halign = halign,
      cut_path = cut_path,
      gp_text = text_gp,
      gp_path = path_gp,
      flip_inverted = flip_inverted,
      default.units = "npc",
      angle = data$angle,
      polar_params = if (inherits(coord, "CoordPolar")) list(x = 0.5, y = 0.5)
                     else NULL
    )
  }
)


