##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_textdensity main                                                    ##
##                                                                           ##
##  Copyright (C) 2021 by Allan Cameron & Teun van den Brand                 ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

# Constructor -------------------------------------------------------------

#' Produce smoothly labelled density plots in \pkg{ggplot2}
#'
#' @description Line plots of smoothed kernel density estimates are available
#'   in \pkg{ggplot2} via [`geom_density`][ggplot2::geom_density]. This geom
#'   layer simply adds a text label to each curve that follow the contour of
#'   the density line when used as a drop-in replacement for
#'   [`geom_density`][ggplot2::geom_density]
#' @inheritParams ggplot2::layer
#' @inheritParams geom_textpath
#' @param bw The smoothing bandwidth to be used.
#'   If numeric, the standard deviation of the smoothing kernel.
#'   If character, a rule to choose the bandwidth, as listed in
#'   [stats::bw.nrd()].
#' @param adjust A multiplicate bandwidth adjustment. This makes it possible
#'    to adjust the bandwidth while still using the a bandwidth estimator.
#'    For example, `adjust = 1/2` means use half of the default bandwidth.
#' @param kernel Kernel. See list of available kernels in [density()].
#' @param n number of equally spaced points at which the density is to be
#'   estimated, should be a power of two, see [density()] for
#'   details
#'
#' @eval rd_aesthetics("geom", "text_density")
#'
#' @export
#' @md
#' @include geom_textpath.R
#' @examples
#' ggplot(iris, aes(Sepal.Length, label = Species, color = Species)) +
#'   geom_textdensity()


geom_textdensity    <- function(mapping = NULL,
                                data = NULL,
                                stat = "density",
                                position = "identity",
                                ...,
                                bw = "nrd0",
                                adjust = 1,
                                kernel = "gaussian",
                                n = 512,
                                cut_path = NA,
                                flip_inverted = TRUE,
                                offset = NULL,
                                keep_straight = FALSE,
                                padding = unit(0.15, "inch"),
                                lineend = "butt",
                                linejoin = "round",
                                linemitre = 10,
                                halign = "center",
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextDensity,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      halign        = halign,
      bw            = bw,
      adjust        = adjust,
      kernel        = kernel,
      n             = n,
      cut_path      = cut_path,
      flip_inverted = flip_inverted,
      offset        = offset,
      keep_straight = keep_straight,
      lineend       = lineend,
      linejoin      = linejoin,
      linemitre     = linemitre,
      na.rm         = na.rm,
      padding       = padding,
      ...
    )
  )
}


#' @rdname geom_textdensity
#' @format NULL
#' @usage NULL
#' @export
#' @include geom_textpath.R
GeomTextDensity <- ggproto("GeomTextDensity",
                      GeomTextpath,
                      required_aes = c("x", "label"),
                      default_aes  = aes(colour     = "black",
                                         size       = 3.88,
                                         hjust      = 0.5,
                                         vjust      = 0.5,
                                         family     = "",
                                         fontface   = 1,
                                         lineheight = 1.2,
                                         alpha      = 1,
                                         linewidth  = 0.5,
                                         linetype   = 1,
                                         spacing    = 0,
                                         linecolour = "_copy_text_colour_",
                                         angle      = 0)
)