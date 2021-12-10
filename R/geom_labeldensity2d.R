##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_labeldensity2d main                                                 ##
##                                                                           ##
##  Copyright (C) 2021 by Allan Cameron & Teun van den Brand                 ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

# Constructor -------------------------------------------------------------

#' Produce labelled contour lines of 2D density in  \pkg{ggplot2}
#'
#' @description Contour lines representing 2D density are available already in
#'   \pkg{ggplot2}, but the native [`geom_density_2d`][ggplot2::geom_density_2d]
#'   does not allow the lines to be labelled with the level of each contour.
#'   `geom_labeldensity2d` adds this ability.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_density_2d
#' @param stat 	The statistical transformation to use on the data for this
#'   layer, as a string
#' @param ... other arguments passed on to [`layer()`][ggplot2::layer].
#'   These are often aesthetics, used to set an aesthetic to a fixed value,
#'   like `colour = "red"` or `size = 3`. They may also be parameters to the
#'   paired geom/stat.
#' @param na.rm If `FALSE` (default), missing points or labels are removed from
#'   the text path with a warning.
#' @param cut_path A `logical(1)` which if `TRUE` breaks the path
#'   into two sections, one on either side of the string. If `FALSE`, the
#'   path is plotted as a whole. The default, `NA`, will break the line if the
#'   string has a `vjust` of between 0 and 1.
#' @param flip_inverted A `logical(1)` which if `TRUE` (default), inverts any
#'   string where the majority of letters would be upside down along the path
#'   are inverted to improve legibility. If `FALSE` letters are left as-is.
#' @param offset A [`unit()`][grid::unit()] of length 1 to determine the offset
#'   of the text from the path. If not `NULL`, this overrules the `vjust`
#'   setting.
#' @param keep_straight a logical **TRUE** or **FALSE** indicating whether the
#'   text should be straight rather than following the curve. This might be
#'   helpful for noisy paths. If **TRUE** the text will still follow the angle
#'   of the curve. The default is **FALSE**
#' @param padding A [`unit()`][grid::unit()] of length 1 to determine the
#'   padding between path and text when the `cut_path` parameter trims the
#'   path.
#'
#' @section Aesthetics:
#' The `spacing` aesthetic allows fine control of spacing of text,
#' which is called 'tracking' in typography. The default is 0 and units are
#' measured in 1/1000 em. Numbers greater than zero increase the spacing,
#' whereas negative numbers decrease the spacing.
#' `geom_labeldensity2d()` understands the following aesthetics (required
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
#' @export
#' @md
#'
#' @examples
#' set.seed(1)
#'
#' df  <- data.frame(x = rnorm(100), y = rnorm(100))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_labeldensity2d() +
#'   theme_classic()


geom_labeldensity2d <- function(mapping = NULL,
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
    geom = GeomLabelDensity2d,
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


#' @rdname geom_labeldensity2d
#' @format NULL
#' @usage NULL
#' @export
GeomLabelDensity2d <- ggproto("GeomLabelDensity2d", GeomTextpath,
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
