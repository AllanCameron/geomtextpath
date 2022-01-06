##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_textcontour.R                                                       ##
##  Part of the geomtextpath R package                                       ##
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
#' @eval rd_dots(geom_textcontour)
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
  bins = NULL, binwidth = NULL, breaks = NULL,
  ...
  ) {

  layer(geom = GeomTextContour, mapping = mapping, data = data,
        stat = stat,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = set_params(
          na.rm     = na.rm,
          lineend   = lineend,
          linejoin  = linejoin,
          linemitre = linemitre,
          bins      = bins,
          binwidth  = binwidth,
          breaks    = breaks,
          ...
        ))
}

#' @rdname geom_textcontour
#' @inheritParams geom_textcontour
#' @inheritParams geom_labelpath
#' @export
geom_labelcontour <- function(
  mapping = NULL, data = NULL, stat = "text_contour",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, ...,
  lineend = "butt", linejoin = "round", linemitre = 10,
  bins = NULL, binwidth = NULL, breaks = NULL,
  label.padding = unit(0.25, "lines"),
  label.r = unit(0.15, "lines"),
  arrow = NULL
) {
  layer(
    geom        = GeomLabelpath,
    mapping     = mapping,
    data        = data,
    stat        = stat,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = set_params(
                na.rm         = na.rm,
                lineend       = lineend,
                linejoin      = linejoin,
                linemitre     = linemitre,
                bins          = bins,
                binwidth      = binwidth,
                breaks        = breaks,
                label.padding = label.padding,
                label.r       = label.r,
                arrow         = arrow,
                ...
              )
  )
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
    data        = data,
    mapping     = mapping,
    stat        = StatTextContour,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = set_params(
                    bins     = bins,
                    binwidth = binwidth,
                    breaks   = breaks,
                    na.rm    = na.rm,
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
  required_aes = c("x", "y")
)

#' @rdname geom_textcontour
#' @format NULL
#' @usage NULL
#' @export
#' @include geom_textpath.R
GeomLabelContour <- ggproto("GeomLabelContour", GeomLabelpath,
  required_aes = c("x", "y")
)

#' @rdname geom_textcontour
#' @format NULL
#' @usage NULL
#' @export
#' @include geom_textpath.R
StatTextContour <- ggproto("StatTextContour", StatContour,

  required_aes = c("x", "y", "z"),

  default_aes  = aes(order = after_stat(level),
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
