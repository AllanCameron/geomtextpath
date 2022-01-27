##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_textcurve.R                                                         ##
##  Part of the geomtextpath R package                                       ##
##                                                                           ##
##  Copyright (C) 2021 - 2022 by Allan Cameron & Teun van den Brand          ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

# Constructors -----------------------------------------------------------------

#' Text on a curve
#'
#' `geom_textcurve()` and `geom_labelcurve()` draw text on curved lines. See
#' the underlying [`grid::curveGrob()`][grid::curveGrob] for the parameters that
#' control the curve.
#'
#' @inheritParams geom_textpath
#' @inheritParams ggplot2::geom_curve
#' @eval rd_dots(geom_textcurve, "text_smoothing")
#' @eval rd_aesthetics("geom", "textcurve")
#'
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#' @md
#'
#' @examples
#' t <- seq(0, 2 * pi, length.out = 4)[-1]
#'
#' df <- data.frame(
#'   x = cos(t),
#'   y = sin(t),
#'   xend = cos(t + 1.8),
#'   yend = sin(t + 1.8)
#' )
#'
#' ggplot(df, aes(x, y, xend = xend, yend = yend)) +
#'   geom_textcurve(
#'     label = c(
#'       "A chicken lays an egg",
#'       "A chick becomes a chicken",
#'       "An egg hatches into a chick"
#'     ),
#'     curvature = 0.5, vjust = 2,
#'     arrow = arrow(ends = "first")
#'   ) +
#'   coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1))
geom_textcurve <- function(
  mapping     = NULL,
  data        = NULL,
  stat        = "identity",
  position    = "identity",
  ...,
  curvature   = 0.5,
  angle       = 90,
  ncp         = 5,
  arrow       = NULL,
  lineend     = "butt",
  na.rm       = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {

  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomTextcurve,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = set_params(
                    arrow     = arrow,
                    curvature = curvature,
                    angle     = angle,
                    ncp       = ncp,
                    lineend   = lineend,
                    na.rm     = na.rm,
                    ...
    )
  )
}


#' @rdname geom_textcurve
#' @export
geom_labelcurve <- function(
    mapping       = NULL,
    data          = NULL,
    stat          = "identity",
    position      = "identity",
    ...,
    curvature     = 0.5,
    angle         = 90,
    ncp           = 5,
    arrow         = NULL,
    lineend       = "butt",
    label.r       = unit(0.15, "lines"),
    label.padding = unit(0.25, "lines"),
    na.rm         = FALSE,
    show.legend   = NA,
    inherit.aes   = TRUE
) {

  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomLabelcurve,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = set_params(
                    arrow     = arrow,
                    curvature = curvature,
                    angle     = angle,
                    ncp       = ncp,
                    lineend   = lineend,
                    na.rm     = na.rm,
                    .type     = "label",
                    ...
                  )
  )
}


# ggproto classes --------------------------------------------------------------

#' @export
#' @rdname GeomTextpath
#' @format NULL
#' @usage NULL
GeomTextcurve <- ggproto(
  "GeomTextcurve", GeomTextpath,

  required_aes = c("x", "xend", "y", "yend", "label"),

  draw_panel = function(
    data, panel_params, coord,
    curvature = 0.5, angle = 90,
    ncp = 5, arrow = NULL, lineend = "butt", na.rm = FALSE,
    text_params = static_text_params("text")
  ) {

    if (!coord$is_linear()) {
      warn("geom_textcurve is not implemented for non-linear coordinates")
    }

    trans   <- coord$transform(data, panel_params)
    text_gp <- data_to_text_gp(data)
    path_gp <- data_to_path_gp(data, lineend = lineend)

    label <- if (text_params$parse) {
      safe_parse(as.character(trans$label))
    } else {
      trans$label
    }

    textcurveGrob(
      x1             = trans$x,
      x2             = trans$xend,
      y1             = trans$y,
      y2             = trans$yend,
      label          = label,
      hjust          = trans$hjust,
      vjust          = text_params$offset %||% trans$vjust,
      halign         = text_params$halign,
      gap            = text_params$gap,
      rich           = text_params$rich,
      gp_text        = text_gp,
      gp_path        = path_gp,
      straight       = text_params$straight,
      upright        = text_params$upright,
      padding        = text_params$padding,
      text_smoothing = text_params$text_smoothing,
      default.units  = "npc",
      arrow          = arrow,
      curvature      = curvature,
      angle          = angle,
      ncp            = ncp,
      square         = FALSE,
      squareShape    = 1,
      inflect        = FALSE,
      open           = TRUE
    )
  }
)


#' @export
#' @rdname GeomTextpath
#' @format NULL
#' @usage NULL
GeomLabelcurve <- ggproto("GeomLabelcurve", GeomLabelpath,

  required_aes = c("x", "xend", "y", "yend", "label"),

  draw_panel = function(
    data, panel_params, coord,
    curvature = 0.5, angle = 90,
    ncp = 5, arrow = NULL, lineend = "butt", na.rm = FALSE,
    text_params = static_text_params("label"),
    label.r = unit(0.15, "lines"), label.padding = unit(0.25, "lines")
  ) {

    if (!coord$is_linear()) {
      warn("geom_labelcurve is not implemented for non-linear coordinates")
    }

    trans <- coord$transform(data, panel_params)

    text_gp <- data_to_text_gp(data)
    path_gp <- data_to_path_gp(data, lineend = lineend)
    box_gp  <- data_to_box_gp(data, lineend = lineend)

    label <- if (text_params$parse) {
      safe_parse(as.character(trans$label))
    } else {
      trans$label
    }

    textcurveGrob(
      x1             = trans$x,
      x2             = trans$xend,
      y1             = trans$y,
      y2             = trans$yend,
      label          = label,
      hjust          = trans$hjust,
      vjust          = text_params$offset %||% trans$vjust,
      gp_text        = text_gp,
      gp_path        = path_gp,
      gp_box         = box_gp,
      halign         = text_params$halign,
      gap            = text_params$gap,
      rich           = text_params$rich,
      straight       = text_params$straight,
      upright        = text_params$upright,
      padding        = text_params$padding,
      text_smoothing = text_params$text_smoothing,
      default.units  = "npc",
      arrow          = arrow,
      as_label       = TRUE,
      curvature      = curvature,
      angle          = angle,
      ncp            = ncp,
      square         = FALSE,
      squareShape    = 1,
      inflect        = FALSE,
      open           = TRUE
    )
  }
)


# Grob constructor -------------------------------------------------------------

textcurveGrob <- function(
    x1, x2, y1, y2,
    label,
    curvature     = 1,
    angle         = 90,
    ncp           = 1,
    square        = FALSE,
    squareShape   = 1,
    inflect       = FALSE,
    open          = TRUE,
    shape         = 0.5,
    ...,
    default.units = "npc",
    name          = NULL,
    vp            = NULL
) {

  # Construct textpathGrob with dummy variables
  n_label  <- length(label)
  dummy_x  <- rep(c(0, 1), n_label)
  dummy_y  <- rep(c(0, 1), n_label)
  dummy_id <- rep(seq_len(n_label), each = 2)

  textgrob <- textpathGrob(
    label,
    dummy_x,
    dummy_y,
    dummy_id,
    ...,
    default.units = default.units
    )

  # Create curveGrob in parallel
  curvegrob <- curveGrob(
    x1, y1, x2, y2,
    default.units = default.units,
    curvature     = curvature,
    angle         = angle,
    ncp           = ncp,
    shape         = shape,
    square        = square,
    squareShape   = squareShape,
    inflect       = inflect,
    open          = open
  )

  gTree(
    textpath = textgrob,
    curve    = curvegrob,
    name     = name,
    vp       = vp,
    cl       = "textcurve"
  )
}


# makeContent ------------------------------------------------------------------

#' @export
makeContent.textcurve <- function(x) {

  # Extract and clear grobs
  text  <- x$textpath
  x$textpath <- NULL

  curve <- x$curve
  x$curve <- NULL

  # Evaluate curve
  curve <- makeContent(curve)$children[[1]]

  # Get points on curve
  if (inherits(curve, "xspline")) {

    # Most of the time, gets evaluated as xspline grob
    pts <- xsplinePoints(curve)
    pts <- if (all(c("x", "y") %in% names(pts))) list(pts) else pts
    xx <- do.call(unit.c, lapply(pts, `[[`, i = "x"))
    yy <- do.call(unit.c, lapply(pts, `[[`, i = "y"))
    id <- vapply(pts, function(x){length(x$x)}, integer(1))
    id <- rep(seq_along(id), id)

  } else if (inherits(curve, "segments")) {

    # When curvature = 0, it is a segments grob
    xx <- unit.c(curve$x0, curve$x1)
    yy <- unit.c(curve$y0, curve$y1)
    i  <- seq_along(curve$x0)
    id <- rep(i, each = 2)
    i  <- `dim<-`(rbind(i, i + max(i), deparse.level = 0), NULL)
    xx <- xx[i]
    yy <- yy[i]

  }

  # Substitute paths by points on curve
  text$textpath$data <- data_frame(
    x = xx, y = yy, id = id, line_x = xx, line_y = yy
  )

  addGrob(x, text)
}
