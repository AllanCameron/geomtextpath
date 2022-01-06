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
#'
#' @section CRS:
#' `coord_sf()` ensures that all layers use a common CRS. You can
#' either specify it using the `crs` param, or `coord_sf()` will
#' take it from the first layer that defines a CRS.
#'
#' @section Combining sf layers and regular geoms:
#' Most regular geoms, such as [geom_point()], [geom_path()],
#' [geom_text()], [geom_polygon()] etc. will work fine with `coord_sf()`.
#' However when using these geoms, two problems arise. First, what CRS should be
#' used for the x and y coordinates used by these non-sf geoms? The CRS applied
#' to non-sf geoms is set by the `default_crs` parameter, and it defaults to
#' `NULL`, which means positions for non-sf geoms are interpreted as projected
#' coordinates in the coordinate system set by the `crs` parameter. This setting
#' allows you complete control over where exactly items are placed on the plot
#' canvas, but it may require some understanding of how projections work and how
#' to generate data in projected coordinates. As an alternative, you can set
#' `default_crs = sf::st_crs(4326)`, the World Geodetic System 1984 (WGS84).
#' This means that x and y positions are interpreted as longitude and latitude,
#' respectively. You can also specify any other valid CRS as the default CRS for
#' non-sf geoms.
#'
#' The second problem that arises for non-sf geoms is how straight lines
#' should be interpreted in projected space when `default_crs` is not set to
#' `NULL`. The approach `coord_sf()` takes is to break straight lines into small
#' pieces (i.e., segmentize them) and then transform the pieces into projected
#' coordinates. For the default setting where x and y are interpreted as
#' longitude and latitude, this approach means that horizontal lines follow the
#' parallels and vertical lines follow the meridians. If you need a different
#' approach to handling straight lines, then you should manually segmentize and
#' project coordinates and generate the plot in projected coordinates.
#'
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'
#'   You can also set this to one of "polygon", "line", and "point" to
#'   override the default legend.
#' @seealso [stat_sf_coordinates()]
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
