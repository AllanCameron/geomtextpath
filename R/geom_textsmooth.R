##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_textsmooth.R                                                        ##
##  Part of the geomtextpath R package                                       ##
##                                                                           ##
##  Copyright (C) 2021 - 2022 by Allan Cameron & Teun van den Brand          ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

#' Labelled conditional means in \pkg{ggplot2}
#'
#' @description Smoothed conditional means are available
#'   in \pkg{ggplot2} via [`geom_smooth`][ggplot2::geom_smooth]. This geom
#'   layer simply adds a text label to each curve that follow the contour of
#'   this line when used as a drop-in replacement for
#'   [`geom_smooth`][ggplot2::geom_smooth]
#' @eval rd_dots(geom_textsmooth)
#' @inheritParams ggplot2::geom_smooth
#' @inheritParams geom_textpath
#' @return A `Layer` ggproto object that can be added to a plot.
#' @seealso Other [geom layers][sibling_layers] that place text on paths.
#' @eval rd_aesthetics("geom", "textdensity")
#'
#' @export
#' @md
#' @include geom_textpath.R
#' @examples
#' ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
#'   geom_point(alpha = 0.1) +
#'   geom_textsmooth(aes(label = Species, colour = Species),
#'                 method = "loess", formula = y ~ x,
#'                 size = 7, linetype = 3, fontface = 2, linewidth = 1) +
#'   scale_colour_manual(values = c("forestgreen", "deepskyblue4", "tomato4")) +
#'   theme_bw() +
#'   theme(legend.position = "none")

geom_textsmooth <- function(
  mapping     = NULL,
  data        = NULL,
  stat        = "smooth",
  position    = "identity",
  ...,
  method      = NULL,
  formula     = NULL,
  na.rm       = FALSE,
  method.args = list(),
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE
) {

  params <- set_params(
    na.rm       = na.rm,
    orientation = orientation,
    method.args = method.args,
    ...
  )
  if (identical(stat, "smooth")) {
    params$method <- method
    params$formula <- formula
  }

  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomTextpath,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = params
  )
}


#' @rdname geom_textsmooth
#' @inheritParams ggplot2::geom_smooth
#' @inheritParams geom_labelpath
#' @export
geom_labelsmooth <- function(
  mapping     = NULL,
  data        = NULL,
  stat        = "smooth",
  position    = "identity",
  method      = NULL,
  formula     = NULL,
  na.rm       = FALSE,
  method.args = list(),
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {

  params <- set_params(
    na.rm       = na.rm,
    orientation = orientation,
    method.args = method.args,
    ...
  )

  if (identical(stat, "smooth")) {
    params$method <- method
    params$formula <- formula
  }

  layer(
    geom        = GeomLabelpath,
    mapping     = mapping,
    data        = data,
    stat        = stat,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = params
  )
}
