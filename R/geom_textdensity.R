##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_textdensity.R                                                       ##
##  Part of the geomtextpath R package                                       ##
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
#' @eval rd_dots(geom_textdensity)
#' @inheritParams geom_textpath
#' @inheritParams ggplot2::stat_density
#'
#' @eval rd_aesthetics("geom", "textdensity")
#' @return A `Layer` ggproto object that can be added to a plot.
#' @seealso Other [geom layers][sibling_layers] that place text on paths.
#' @export
#' @md
#' @include geom_textpath.R
#' @examples
#' ggplot(iris, aes(Sepal.Length, label = Species, color = Species)) +
#'   geom_textdensity()

geom_textdensity <- function(mapping = NULL,
                             data = NULL,
                             stat = "density",
                             position = "identity",
                             ...,
                             bw = "nrd0",
                             adjust = 1,
                             kernel = "gaussian",
                             n = 512,
                             lineend = "butt",
                             linejoin = "round",
                             linemitre = 10,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomTextdensity,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = set_params(
                    bw            = bw,
                    adjust        = adjust,
                    kernel        = kernel,
                    n             = n,
                    lineend       = lineend,
                    linejoin      = linejoin,
                    linemitre     = linemitre,
                    na.rm         = na.rm,
                    ...
                  )
  )
}

#' @rdname geom_textdensity
#' @inheritParams geom_textdensity
#' @inheritParams geom_labelpath
#' @export
geom_labeldensity <- function(mapping = NULL, data = NULL,
  stat = "density", position = "identity",
  na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE,
  ...,
  lineend = "butt", linejoin = "round", linemitre = 10,
  bw = "nrd0",
  adjust = 1,
  kernel = "gaussian",
  n = 512,
  label.padding = unit(0.25, "lines"),
  label.r = unit(0.15, "lines"),
  arrow = NULL
) {
  layer(
    geom        = GeomLabeldensity,
    mapping     = mapping,
    data        = data,
    stat        = stat,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = set_params(
                    bw            = bw,
                    adjust        = adjust,
                    kernel        = kernel,
                    n             = n,
                    na.rm         = na.rm,
                    lineend       = lineend,
                    linejoin      = linejoin,
                    linemitre     = linemitre,
                    label.padding = label.padding,
                    label.r       = label.r,
                    arrow         = arrow,
                    ...
                  )
  )
}


#' @rdname GeomTextpath
#' @format NULL
#' @usage NULL
#' @export
#' @include geom_textpath.R
GeomTextdensity <- ggproto("GeomTextdensity", GeomTextpath,

  required_aes = c("x", "label")
)


#' @rdname GeomTextpath
#' @format NULL
#' @usage NULL
#' @export
#' @include geom_textpath.R
GeomLabeldensity <- ggproto("GeomLabeldensity", GeomLabelpath,

  required_aes = c("x", "label")
)
