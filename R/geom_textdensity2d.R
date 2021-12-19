##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_textdensity2d main                                                  ##
##                                                                           ##
##  Copyright (C) 2021 by Allan Cameron & Teun van den Brand                 ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

#' Produce labelled contour lines of 2D density in  \pkg{ggplot2}
#'
#' @description Contour lines representing 2D density are available already in
#'   \pkg{ggplot2}, but the native [`geom_density_2d`][ggplot2::geom_density_2d]
#'   does not allow the lines to be labelled with the level of each contour.
#'   `geom_textdensity2d` adds this ability.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams geom_textpath
#' @inheritParams ggplot2::stat_density_2d
#'
#' @eval rd_aesthetics("geom", "text_density2d")
#'
#' @include geom_textpath.R
#' @include utils.R
#' @export
#' @md
#'
#' @examples
#' set.seed(1)
#'
#' df  <- data.frame(x = rnorm(100), y = rnorm(100))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_textdensity2d() +
#'   theme_classic()


geom_textdensity2d <- function(mapping = NULL,
                                data = NULL,
                                stat = "density_2d",
                                position = "identity",
                                ...,
                                gap = NA,
                                upright = TRUE,
                                offset = NULL,
                                straight = FALSE,
                                padding = unit(0.15, "inch"),
                                contour_var = "density",
                                n = 100,
                                h = NULL,
                                adjust = c(1, 1),
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
    geom        = GeomTextDensity2d,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(gap         = gap,
                       upright     = upright,
                       offset      = offset,
                       straight    = straight,
                       lineend     = lineend,
                       linejoin    = linejoin,
                       linemitre   = linemitre,
                       contour     = TRUE,
                       contour_var = contour_var,
                       na.rm       = na.rm,
                       padding     = padding,
                       n           = n,
                       h           = h,
                       adjust      = adjust,
                       ...
    )
  )
}


#' @rdname geom_textdensity2d
#' @format NULL
#' @usage NULL
#' @export
#' @include geom_textpath.R
GeomTextDensity2d <- ggproto("GeomTextDensity2d", GeomTextpath,
  required_aes = c("x", "y"),
  default_aes = aes(colour = "black",
                    size = 3.88,
                    hjust = 0.5,
                    vjust = 0.5,
                    family = "",
                    fontface = 1,
                    lineheight = 1.2,
                    alpha = 1,
                    linewidth = 0.5,
                    linetype = 1,
                    spacing = 0,
                    linecolour = "_copy_text_colour_",
                    angle = 0),

  setup_data = function(data, params) {

    data$label <- as.character(data$level)
    data
  }
)
