##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_labelcontour main                                                   ##
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
#'   be labelled with the level of each contour. `geom_labelcontour` adds this
#'   ability.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_contour
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
#'
#' @section Aesthetics:
#' The `spacing` aesthetic allows fine control of spacing of text,
#' which is called 'tracking' in typography. The default is 0 and units are
#' measured in 1/1000 em. Numbers greater than zero increase the spacing,
#' whereas negative numbers decrease the spacing.
#' `geom_labelcontour()` understands the following aesthetics (required
#' aesthetics are in bold):
#' \itemize{
#'   \item \strong{`x`}
#'   \item \strong{`y`}
#'   \item \strong{`z`}
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
#' stat_density2d(geom = "textpath", aes(label = after_stat(level))) +
#'  theme_classic()


geom_labelcontour <- function(
  mapping = NULL, data = NULL, stat = "label_contour",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE,
  lineend = "butt", linejoin = "round", linemitre = 10,
  cut_path = NA, flip_inverted = TRUE,
  offset = NULL, keep_straight = FALSE, bins = NULL,
  binwidth = NULL, breaks = NULL, ...
  )
{
  layer(geom = GeomLabelContour, mapping = mapping, data = data,
        stat = stat,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
          na.rm         = na.rm,
          lineend       = lineend,
          linejoin      = linejoin,
          linemitre     = linemitre,
          cut_path      = cut_path,
          flip_inverted = flip_inverted,
          offset        = offset,
          keep_straight = keep_straight,
          bins          = bins,
          binwidth      = binwidth,
          breaks        = breaks,
          ...
        ))
}

#' @rdname geom_labelcontour
#' @format NULL
#' @usage NULL
#' @export
#' @include geom_textpath.R
stat_labelcontour <- function(mapping = NULL, data = NULL,
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
    stat = StatLabelContour,
    geom = GeomLabelContour,
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

#' @rdname geom_labelcontour
#' @format NULL
#' @usage NULL
#' @export
#' @include geom_textpath.R
GeomLabelContour <- ggproto("GeomLabelContour", GeomTextpath,
  required_aes = c("x", "y"),
  default_aes = aes(colour = "black", size = 3.88, hjust = 0.5, vjust = 0.5,
                    family = "", fontface = 1, lineheight = 1.2, alpha = 1,
                    linewidth = 0.5, linetype = 1, spacing = 0,
                    linecolour = "_copy_text_colour_", angle = 0)
)

#' @rdname geom_labelcontour
#' @format NULL
#' @usage NULL
#' @export
StatLabelContour <- ggproto("StatLabelContour", StatContour,

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

