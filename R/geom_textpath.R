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
#' @inheritParams ggplot2::geom_path
#' @inheritParams static_text_params
#' @param orientation The orientation of the layer. The default (NA)
#'    automatically determines the orientation from the aesthetic mapping.
#'    In the rare event that this fails it can be given explicitly by
#'    setting orientation to either "x" or "y". See the Orientation section
#'    for more detail.
#'
#' @details
#' ## Limitations
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
#' ## Rich text
#' The rich text option (`rich = TRUE`) relies heavily on rich-text parsers
#' copied from the \{\pkg{gridtext}\} package. We thank Claus O. Wilke for
#' developing \{\pkg{gridtext}\} and allowing us to re-use his code under the
#' MIT licence. Currently, the supported HTML tags are `<p>`, `<span>`, `<b>`,
#' `<strong>`, `<i>`, `<em>`, `<sub>`, `<sup>` and `<br>`.
#'
#'
#' @eval rd_aesthetics("geom", "textpath")
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
#' # Use geom_textline as a drop-in for geom_line
#'
#'  df <- data.frame(x = rep(1:100, 3),
#'                   y = sin(c(seq(0, pi, len = 100),
#'                             seq(pi, 2*pi, len = 100),
#'                             rep(0, 100))),
#'                   label = rep(c("y is increasing",
#'                                 "y is falling",
#'                                 "y is flat"), each = 100))
#'
#' ggplot(df, aes(x, y, label = label, color = label)) +
#'    geom_textline(size = 6) + theme(legend.position = "none")


geom_textpath <- function(
  mapping = NULL, data = NULL, stat = "identity",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE,  ...,
  lineend = "butt", linejoin = "round", linemitre = 10,
  text_only = FALSE, gap = NA, upright = TRUE,
  halign = "center", offset = NULL, parse = FALSE, straight = FALSE,
  padding = unit(0.15, "inch"), text_smoothing = 0, rich = FALSE, arrow = NULL
  )
{
  layer(geom = GeomTextpath, mapping = mapping, data = data, stat = stat,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = set_params(
          na.rm          = na.rm,
          lineend        = lineend,
          linejoin       = linejoin,
          linemitre      = linemitre,
          text_only      = text_only,
          gap            = gap,
          upright        = upright,
          halign         = halign,
          offset         = offset,
          parse          = parse,
          straight       = straight,
          padding        = padding,
          text_smoothing = text_smoothing,
          rich           = rich,
          arrow          = arrow,
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

  extra_params = c("na.rm"),

  setup_data = function(data, params) {
    if (isTRUE(params$text_params$text_only)) {
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
    text_params = static_text_params("text"), arrow = NULL
  ) {


    #---- type conversion, checks & warnings ---------------------------#

    copy_colour <- data$linecolour == "_copy_text_colour_"
    data$linecolour[copy_colour] <- data$colour[copy_colour]

    # We need to change groups to numeric to order them appropriately
    data$group <- discretise(data$group)

    # If there is more than one text string associated with any of the groups,
    # we warn that only the first is used
    if (!all(gapply(data$label, data$group,
                   function(x) all(x == x[1]), logical(1))))
    {
         warn(paste("geom_textpath: Multiple strings found in at",
         "least one group. Only the first will be used."))
    }

    #---- Data manipulation ---------------------------------#

    # Now we can sort the data by group
    data <- data[order(data$group), , drop = FALSE]

    # All our transformations occur after the coord transform:
    data <- coord_munch(coord, data, panel_params)

    #---- Set graphical parameters --------------------------#

    # Get first observation of each group
    first <- run_start(data$group)

    text_gp <- gpar(
      col  = alpha(data$colour, data$alpha)[first],
      fontsize   = data$size[first] * .pt,
      fontface   = data$fontface[first],
      fontfamily = data$family[first],
      lineheight = data$lineheight[first],
      tracking   = data$spacing[first]
    )

    if (all(data$linetype %in% c("0", "blank", NA))) {
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

    safe_labels <- if (text_params$parse) {
        safe_parse(as.character(data$label[first]))
      } else {
        data$label[first]
      }

    #---- Dispatch data to grob -----------------------------#

    textpathGrob(
      label = safe_labels,
      x = data$x,
      y = data$y,
      id = data$group,
      hjust  = data$hjust[first],
      vjust  = text_params$offset %||% data$vjust[first],
      halign = text_params$halign,
      gap    = text_params$gap,
      gp_text  = text_gp,
      gp_path  = path_gp,
      straight = text_params$straight,
      upright  = text_params$upright,
      text_smoothing = text_params$text_smoothing,
      default.units = "npc",
      angle = data$angle,
      polar_params = if (inherits(coord, "CoordPolar")){
                       list(x = 0.5, y = 0.5, theta = coord$theta)
                     } else NULL,
      padding = text_params$padding,
      rich    = text_params$rich,
      arrow = arrow
    )
  }
)

#' @export
#' @rdname geom_textpath
geom_textline <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE, orientation = NA,
                      show.legend = NA, inherit.aes = TRUE, ...,
                      lineend = "butt", linejoin = "round", linemitre = 10,
                      arrow = NULL) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = set_params(
          na.rm         = na.rm,
          lineend       = lineend,
          linejoin      = linejoin,
          linemitre     = linemitre,
          arrow         = arrow,
          ...
        )
  )
}

#' The Geom object for a textpath
#'
#' This is the \code{ggproto} class that creates the textline layer. It is not
#' intended to be used directly by the end user.
#'
#' @format NULL
#' @usage NULL
#' @export

GeomTextLine <- ggproto("GeomTextLine", GeomTextpath,
  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    params
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data <- data[order(data$PANEL, data$group, data$x), ]
    flip_data(data, params$flipped_aes)
  }
)
