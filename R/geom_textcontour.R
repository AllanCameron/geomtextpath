##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_textcontour main                                                   ##
##                                                                           ##
##  Copyright (C) 2021 by Allan Cameron & Teun van den Brand                 ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

# Constructor -------------------------------------------------------------

#' Produce labelled contour lines in  \pkg{ggplot2}
#'
#' @description Contour lines are available already in \pkg{ggplot2}, but the
#'   native [`geom_contour`][ggplot2::geom_contour] does not allow the lines to
#'   be labelled with the level of each contour. `geom_textcontour` adds this
#'   ability.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_contour
#' @inheritParams geom_textpath
#' @eval rd_aesthetics("geom", "text_contour")
#'
#' @include geom_textpath.R
#' @export
#' @md
#'
#' @examples
#' df <- expand.grid(x = seq(nrow(volcano)), y = seq(ncol(volcano)))
#' df$z <- as.vector(volcano)
#'
#' ggplot(df, aes(x, y, z = z)) +
#'   geom_contour_filled(bins = 6, alpha = 0.6) +
#'   geom_textcontour(bins = 6, size = 2.5, padding = unit(0.05, "in")) +
#'   scale_fill_manual(values = terrain.colors(11)) +
#'   theme_classic() +
#'   theme(legend.position = "none")
geom_textcontour <- function(
  mapping = NULL, data = NULL, stat = "text_contour",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE,
  lineend = "butt", linejoin = "round", linemitre = 10,
  gap = NA, flip_inverted = TRUE,
  offset = NULL, keep_straight = FALSE, bins = NULL,
  binwidth = NULL, breaks = NULL, padding = unit(0.15, "inch"),
  ...
  )
{
  layer(geom = GeomTextContour, mapping = mapping, data = data,
        stat = stat,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
          na.rm         = na.rm,
          lineend       = lineend,
          linejoin      = linejoin,
          linemitre     = linemitre,
          gap           = gap,
          flip_inverted = flip_inverted,
          offset        = offset,
          keep_straight = keep_straight,
          bins          = bins,
          binwidth      = binwidth,
          breaks        = breaks,
          padding       = padding,
          ...
        ))
}


#' @inheritParams geom_textcontour
#' @param geom The geometric object to use display the data
#' @export
#' @section Computed variables:
#'  The variable `level` is a numeric or a factor
#'   depending on whether lines or bands are calculated.
#' \describe{
#'  \item{`level`}{Height of contour. This is a numeric vector that
#'    represents bin boundaries.
#'   }
#' }
#'
#'
#' @rdname geom_textcontour
stat_textcontour <- function(mapping = NULL, data = NULL,
                             geom = "text_contour",
                             position = "identity",
                             ...,
                             bins = NULL,
                             binwidth = NULL,
                             breaks = NULL,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatTextContour,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      binwidth = binwidth,
      breaks = breaks,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_textcontour
#' @format NULL
#' @usage NULL
#' @export
#' @include geom_textpath.R
GeomTextContour <- ggproto("GeomTextContour", GeomTextpath,
  required_aes = c("x", "y"),
  default_aes = aes(colour = "black", size = 3.88, hjust = 0.5, vjust = 0.5,
                    family = "", fontface = 1, lineheight = 1.2, alpha = 1,
                    linewidth = 0.5, linetype = 1, spacing = 0,
                    linecolour = "_copy_text_colour_", angle = 0)
)

#' @rdname geom_textcontour
#' @format NULL
#' @usage NULL
#' @export
#' @include geom_textpath.R
StatTextContour <- ggproto("StatTextContour", StatContour,

  required_aes = c("x", "y", "z"),
  default_aes = aes(order = after_stat(level),
                    label = round(after_stat(level), 1)),

  setup_params = function(data, params) {
    params$z.range <- range(data$z, na.rm = TRUE, finite = TRUE)
    params
  },

  compute_group = function(self, data, scales, z.range, bins = NULL,
                           binwidth = NULL, breaks = NULL, na.rm = FALSE) {


     data <- ggproto_parent(StatContour, self)$compute_group(data, scales,
                            z.range, bins, binwidth, breaks, na.rm)
     data
  }
)

