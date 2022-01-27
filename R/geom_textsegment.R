##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_textsegment.R                                                       ##
##  Part of the geomtextpath R package                                       ##
##                                                                           ##
##  Copyright (C) 2021 - 2022 by Allan Cameron & Teun van den Brand          ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

# Constructor -------------------------------------------------------------

#' Add text to line segments
#'
#' `geom_textsegment` draws a line between two points defined by (x, y) and
#' (xend, yend) and places a text label on that line. It is the text-placement
#' equivalent of [`geom_segment()`][ggplot2::geom_segment].
#'
#' @eval rd_dots(geom_textsegment)
#' @inheritParams geom_textpath
#'
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#' @md
#' @eval rd_aesthetics("geom", "textsegment")
#' @seealso Other [geom layers][sibling_layers] that place text on paths.
#'
#' @examples
#' # The convenience here is that the position and angle
#' # are in sync automatically with the data
#' sleep2 <- reshape(sleep, direction = "wide",
#'                   idvar = "ID", timevar = "group")
#'
#' ggplot(sleep2, aes(x = "Drug 1", y = extra.1)) +
#'   geom_textsegment(
#'     aes(xend = "Drug 2", yend = extra.2,
#'         label = paste0("Patient #", ID))
#'   )
#'
#' # As an annotation
#' ggplot(mapping = aes(x, y)) +
#'   geom_col(
#'     data = data.frame(x = c(1, 2), y = c(1, 10))
#'   ) +
#'   annotate(
#'     "textsegment",
#'     x = 1, xend = 2, y = 1, yend = 10,
#'     label = "10x increase", arrow = arrow()
#'   )
geom_textsegment <- function(
  mapping     = NULL,
  data        = NULL,
  stat        = "identity",
  position    = "identity",
  ...,
  arrow       = NULL,
  lineend     = "butt",
  linejoin    = "round",
  na.rm       = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  straight    = NULL
) {

  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomTextsegment,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = set_params(
                    arrow     = arrow,
                    na.rm     = na.rm,
                    lineend   = lineend,
                    linejoin  = linejoin,
                    straight  = straight,
                    ...
    )
  )
}


#' @export
#' @rdname geom_textsegment
geom_labelsegment <- function(
  mapping  = NULL,
  data     = NULL,
  stat     = "identity",
  position = "identity",
  ...,
  arrow    = NULL,
  lineend  = "butt",
  linejoin = "round",
  na.rm    = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  straight    = NULL,
  label.r     = unit(0.15, "lines"),
  label.padding = unit(0.25, "lines")
) {

  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomLabelsegment,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = set_params(
                    arrow     = arrow,
                    na.rm     = na.rm,
                    lineend   = lineend,
                    linejoin  = linejoin,
                    straight  = straight,
                    label.r   = label.r,
                    label.padding = label.padding,
                    ...
    )
  )
}


# ggproto class -----------------------------------------------------------

#' @format NULL
#' @usage NULL
#' @export
#' @rdname GeomTextpath
GeomTextsegment <- ggproto("GeomTextsegment", GeomTextpath,

  required_aes = c("x", "y", "xend", "yend", "label"),

  draw_panel = function(
    data, panel_params, coord,
    lineend = "butt", linejoin = "round", linemitre = 10,
    text_params = static_text_params("text"), arrow = NULL
  ) {
    segment2path(
      data = data, super = GeomTextpath, params = text_params,
      coord = coord, panel_params = panel_params, arrow = arrow,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre
    )
  }
)


#' @format NULL
#' @usage NULL
#' @export
#' @rdname GeomTextpath
GeomLabelsegment <- ggproto("GeomLabelsegment", GeomLabelpath,

  required_aes = c("x", "y", "xend", "yend", "label"),

  draw_panel = function(
    data, panel_params, coord,
    lineend = "butt", linejoin = "round", linemitre = 10,
    text_params = static_text_params("label"), arrow = NULL,
    label.padding = unit(0.25, "lines"),
    label.r = unit(0.15, "lines")
  ) {
    segment2path(
      data = data, super = GeomLabelpath, params = text_params,
      coord = coord, panel_params = panel_params, arrow = arrow,
      lineend = lineend, linejoin = linejoin, linemitre = linemitre,
      label.padding = label.padding, label.r = label.r
    )
  }
)


# Helper ------------------------------------------------------------------

#' Since the text/label variants are *almost* identical but have different
#' parents, we define this function in order to avoid code duplication.
#' @param data The same data as in the `draw_panel()` method.
#' @param super The parent of the segments ggproto
#' @param params The text parameters
#' @param coord The coordinates
#' @param ... Other arguments
#' @noRd
segment2path <- function(data, super, params, coord, ...) {

  if (is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
    return(zeroGrob())
  }
  if (is.null(params$straight)) {
    params$straight <- coord$is_linear()
  }
  data$group <- seq_len(nrow(data))
  first <- data[, setdiff(colnames(data), c("xend", "yend")), drop = FALSE]
  final <- data[, setdiff(colnames(data), c("x",    "y")),    drop = FALSE]

  rename <- match(c("xend", "yend"), colnames(final))
  colnames(final)[rename] <- c("x", "y")

  pieces <- rbind(first, final)
  pieces <- pieces[order(pieces$group), , drop = FALSE]

  super$draw_panel(
    pieces, coord, text_params = params, ...
  )
}
