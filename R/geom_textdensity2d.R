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
#' @inheritParams ggplot2::geom_density_2d
#' @inheritParams geom_textpath
#' @param ... other arguments passed on to [`layer()`][ggplot2::layer].
#'   These are often aesthetics, used to set an aesthetic to a fixed value,
#'   like `colour = "red"` or `size = 3`. They may also be parameters to the
#'   paired geom/stat.
#'
#' @section Aesthetics:
#' The `spacing` aesthetic allows fine control of spacing of text,
#' which is called 'tracking' in typography. The default is 0 and units are
#' measured in 1/1000 em. Numbers greater than zero increase the spacing,
#' whereas negative numbers decrease the spacing.
#' `geom_textdensity2d()` understands the following aesthetics (required
#' aesthetics are in bold):
#' \itemize{
#'   \item \strong{`x`}
#'   \item \strong{`y`}
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
#' @include geom_textpath.R
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
                                cut_path = NA,
                                flip_inverted = TRUE,
                                offset = NULL,
                                keep_straight = FALSE,
                                padding = unit(0.15, "inch"),
                                contour_var = "density",
                                lineend = "butt",
                                linejoin = "round",
                                linemitre = 10,
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextDensity2d,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      cut_path      = cut_path,
      flip_inverted = flip_inverted,
      offset        = offset,
      keep_straight = keep_straight,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      contour = TRUE,
      contour_var = contour_var,
      na.rm = na.rm,
      padding = padding,
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
  default_aes = aes(colour = "black", size = 3.88, hjust = 0.5, vjust = 0.5,
    family = "", fontface = 1, lineheight = 1.2, alpha = 1, linewidth = 0.5,
    linetype = 1, spacing = 0, linecolour = "_copy_text_colour_",
    angle = 0),

  setup_data = function(data, params) {

    data$label <- as.character(data$level)
    data
  }


)
