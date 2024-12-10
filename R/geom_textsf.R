##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_textsf.R                                                            ##
##  Part of the geomtextpath R package                                       ##
##                                                                           ##
##  Copyright (C) 2021 - 2022 by Allan Cameron & Teun van den Brand          ##
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
#' @section Geometry aesthetic:
#' `geom_textsf()` uses a unique aesthetic: `geometry`, giving an
#' column of class `sfc` containing simple features data. There
#' are three ways to supply the `geometry` aesthetic:
#'
#'   - Do nothing: by default `geom_textsf()` assumes it is stored in
#'     the `geometry` column.
#'   - Explicitly pass an `sf` object to the `data` argument.
#'     This will use the primary geometry column, no matter what it's called.
#'   - Supply your own using `aes(geometry = my_column)`
#'
#' Unlike other aesthetics, `geometry` will never be inherited from
#' the plot.
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
#' @inheritDotParams geom_textpath -arrow -lineend -linejoin -linemitre
#' @md
geom_textsf <- function(
  mapping     = aes(),
  data        = NULL,
  stat        = "sf",
  position    = "identity",
  na.rm       = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {

  rlang::check_installed("sf", "for `geom_textsf()`")
  c(
    layer_sf(
      geom        = GeomTextsf,
      data        = data,
      mapping     = mapping,
      stat        = stat,
      position    = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params      = set_params(na.rm = na.rm, ...)
    ),
    coord_sf(default = TRUE)
  )
}


#' @export
#' @rdname geom_textsf
#' @inheritParams ggplot2::geom_point
#' @inheritDotParams geom_labelpath -arrow -lineend -linejoin -linemitre
geom_labelsf <- function(
  mapping     = aes(),
  data        = NULL,
  stat        = "sf",
  position    = "identity",
  na.rm       = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {

  rlang::check_installed("sf", "for `geom_labelsf()`")
  c(
    layer_sf(
      geom        = GeomLabelsf,
      data        = data,
      mapping     = mapping,
      stat        = stat,
      position    = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params      = set_params(na.rm = na.rm, ...)
    ),
    coord_sf(default = TRUE)
  )
}


#' @export
#' @rdname GeomTextpath
#' @usage NULL
#' @format NULL
GeomTextsf <- ggproto("GeomTextSf", GeomSf,

  required_aes = c("geometry", "label"),

  default_aes = aes(
    shape      = NULL,
    colour     = "gray30",
    fill       = "gray90",
    linetype   = 1,
    stroke     = 0.5,
    size       = 3.88,
    hjust      = 0.5,
    vjust      = 0.5,
    family     = "",
    fontface   = 1,
    lineheight = 1.2,
    alpha      = 1,
    linewidth  = 0.5,
    spacing    = 0,
    linecolour = "gray30",
    angle      = 0),

  draw_panel = function(data, panel_params, coord, legend = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        text_params = static_text_params("text"),
                        arrow = NULL, na.rm = TRUE) {
    if (!inherits(coord, "CoordSf")) {
      abort("geom_sf() must be used with coord_sf()")
    }

    data <- coord$transform(data, panel_params)
    sf_textgrob(data, lineend = lineend, linejoin = linejoin,
                   linemitre = linemitre, text_params = text_params,
                   arrow = arrow, na.rm = na.rm)
  }
)


#' @export
#' @rdname GeomTextpath
#' @usage NULL
#' @format NULL
GeomLabelsf <- ggproto("GeomLabelSf", GeomSf,

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
                        arrow = NULL, text_params = static_text_params("labels"),
                        na.rm = TRUE) {
    if (!inherits(coord, "CoordSf")) {
      abort("geom_sf() must be used with coord_sf()")
    }

    data <- coord$transform(data, panel_params)
    sf_textgrob(data, lineend = lineend, linejoin = linejoin,
                linemitre = linemitre, text_params = text_params,
                arrow = arrow, na.rm = na.rm, as_textbox = TRUE)
  }
)
