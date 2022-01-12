##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_textsf.R                                                            ##
##  Part of the geomtextpath R package                                       ##
##                                                                           ##
##  Copyright (C) 2021 by Allan Cameron & Teun van den Brand                 ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

#' Visualise sf objects with labels
#'
#' This set of geom, stat, and coord are used to visualise simple feature (sf)
#' objects. For simple plots, you will only need `geom_sf()` as it
#' uses `stat_sf()` and adds `coord_sf()` for you. `geom_textsf()` is
#' an unusual geom because it will draw different geometric objects depending
#' on what simple features are present in the data: you can get points, lines,
#' or polygons.
#'
#' @inheritSection ggplot2::geom_sf Geometry aesthetic
#' @inheritSection ggplot2::geom_sf CRS
#' @inheritSection ggplot2::geom_sf Combining sf layers and regular geoms
#'
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'
#'   You can also set this to one of "polygon", "line", and "point" to
#'   override the default legend.
#'
#' @return A `Layer` ggproto object that can be added to a plot.
#' @seealso [`stat_sf_coordinates()`][ggplot2::stat_sf_coordinates].
#' Other [geom layers][sibling_layers] that place text on paths.
#' @md
#' @examples
#' ggplot(waterways) +
#'  geom_textsf(label = "Forth and Clyde Canal",
#'               hjust = 0.62, vjust = -0.3, fill = "#E4E0A3") +
#'  lims(x = c(-4.2, -3.9), y = c(55.9, 56))
#'
#' @name geom_textsf
NULL



#' @export
#' @rdname geom_textsf
#' @inheritParams ggplot2::geom_point
#' @param ... Extra arguments passed to [`geom_textpath`][geom_textpath]
geom_textsf <- function(mapping = aes(), data = NULL, stat = "sf",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  c(
    layer_sf(
      geom = GeomTextSf,
      data = data,
      mapping = mapping,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        ...
      )
    ),
    coord_sf(default = TRUE)
  )
}

#' @export
#' @rdname geom_textsf
#' @inheritParams ggplot2::geom_point
#' @inheritParams geom_labelpath
#' @param ... Extra arguments passed to [`geom_textpath`][geom_textpath]
geom_labelsf <- function(mapping = aes(), data = NULL, stat = "sf",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  c(
    layer_sf(
      geom = GeomLabelSf,
      data = data,
      mapping = mapping,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        ...
      )
    ),
    coord_sf(default = TRUE)
  )
}

#' @export
#' @rdname geom_textsf
#' @usage NULL
#' @format NULL
GeomTextSf <- ggproto("GeomTextSf", GeomSf,
  required_aes = c("geometry", "label"),
  default_aes = aes(
    shape = NULL,
    colour = "gray30",
    fill = "gray90",
    linetype = 1,
    stroke = 0.5,
    size = 3.88,
    hjust = 0.5,
    vjust = 0.5,
    family = "",
    fontface = 1,
    lineheight = 1.2,
    alpha = 1,
    linewidth = 0.5,
    spacing = 0,
    linecolour = "gray30",
    angle = 0),

  draw_panel = function(data, panel_params, coord, legend = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        text_smoothing = 0,
                        arrow = NULL, na.rm = TRUE) {
    if (!inherits(coord, "CoordSf")) {
      abort("geom_sf() must be used with coord_sf()")
    }

    data <- coord$transform(data, panel_params)
    sf_textgrob(data, lineend = lineend, linejoin = linejoin,
                   linemitre = linemitre, text_smoothing = text_smoothing,
                   arrow = arrow, na.rm = na.rm)
  }
)


#' @export
#' @rdname geom_textsf
#' @usage NULL
#' @format NULL
GeomLabelSf <- ggproto("GeomLabelSf", GeomSf,
  required_aes = c("geometry", "label"),
  default_aes = aes(
    colour       = "black",
    alpha        = 1,
    size         = 3.88,
    hjust        = 0.5,
    vjust        = 0.5,
    family       = "",
    fontface     = 1,
    lineheight   = 1.2,
    linewidth    = 0.5,
    linetype     = 1,
    spacing      = 0,
    linecolour   = NULL,
    angle        = 0,
    fill         = "white",
    boxfill      = "white",
    boxcolour    = NULL,
    boxlinetype  = 1,
    boxlinewidth = NULL
  ),

  draw_panel = function(data, panel_params, coord, legend = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        arrow = NULL, text_smoothing = 0, na.rm = TRUE) {
    if (!inherits(coord, "CoordSf")) {
      abort("geom_sf() must be used with coord_sf()")
    }

    data <- coord$transform(data, panel_params)
    sf_textgrob(data, lineend = lineend, linejoin = linejoin,
                   linemitre = linemitre, text_smoothing = text_smoothing,
                   arrow = arrow, na.rm = na.rm, as_textbox = TRUE)
  }
)
