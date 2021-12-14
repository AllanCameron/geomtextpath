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

#' @export
#' @rdname geom_textpath
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
geom_labelpath <- function(
  mapping = NULL, data = NULL,
  stat = "identity", position = "identity",
  na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE,
  ...,
  lineend = "butt", linejoin = "round", linemitre = 10,
  include_line = TRUE, cut_path = FALSE, flip_inverted = TRUE,
  halign = "left", offset = NULL, parse = FALSE,
  keep_straight = FALSE,
  padding = unit(0.15, "inch"),
  label.padding = unit(0.25, "lines"),
  label.r = unit(0.15, "lines")
) {
  layer(
    geom        = GeomLabelpath,
    mapping     = mapping,
    data        = data,
    stat        = stat,
    position    = position,
    show.legend = show.legend,
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
      parse         = parse,
      keep_straight = keep_straight,
      padding       = padding,
      label.padding = label.padding,
      label.r       = label.r,
      ...
    )
  )
}

#' @export
#' @rdname GeomTextpath
#' @format NULL
#' @usage NULL
#' @include geom_textpath.R
GeomLabelpath <- ggproto(
  "GeomLabelpath", GeomTextpath,

  default_aes = aes(colour = "black", size = 3.88, hjust = 0.5, vjust = 0.5,
                    family = "", fontface = 1, lineheight = 1.2, alpha = 1,
                    linewidth = 0.5, linetype = 1, spacing = 0,
                    linecolour = "_copy_text_colour_", angle = 0,
                    fill = "white"),

  draw_panel = function(
    data, panel_params, coord,
    lineend = "butt", linejoin = "round", linemitre = 10,
    cut_path = NA, flip_inverted = TRUE, halign = "left",
    offset = NULL, parse = FALSE, keep_straight = FALSE,
    padding = unit(0.15, "inch"), label.padding = unit(0.25, "lines"),
    label.r = unit(0.15, "lines")
  ) {


    #---- type conversion, checks & warnings ---------------------------#

    copy_colour <- data$linecolour == "_copy_text_colour_"
    data$linecolour[copy_colour] <- data$colour[copy_colour]

    # We need to change groups to numeric to order them appropriately
    data$group <- discretise(data$group)

    # If there is more than one text string associated with any of the groups,
    # we warn that only the first is used
    if(!all(vapply(split(data$label, data$group),
                   function(x) all(x == x[1]), logical(1))))
    {
      warn(paste("geom_labelpath: Multiple strings found in at",
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
        fill = alpha(data$fill, data$alpha)[first],
        lwd  = data$linewidth[first] * .pt,
        lty  = data$linetype[first],
        lineend   = lineend,
        linejoin  = linejoin,
        linemitre = linemitre
      )
    }

    safe_labels <- if(parse) {
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
      vjust  = offset %||% data$vjust[first],
      halign = halign,
      cut_path = cut_path,
      gp_text  = text_gp,
      gp_path  = path_gp,
      gp_box   = path_gp,
      keep_straight = keep_straight,
      flip_inverted = flip_inverted,
      default.units = "npc",
      angle = data$angle,
      polar_params = if (inherits(coord, "CoordPolar")){
        list(x = 0.5, y = 0.5, theta = coord$theta)
      } else NULL,
      padding = padding,
      label.padding = label.padding,
      label.r = label.r
    )
  }
)
