##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_textabline.R                                                        ##
##  Part of the geomtextpath R package                                       ##
##                                                                           ##
##  Copyright (C) 2021 by Allan Cameron & Teun van den Brand                 ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

#' Labelled reference lines: horizontal, vertical, and diagonal
#'
#' These geoms add labelled reference lines to a plot, either
#' horizontal, vertical, or diagonal (specified by slope and intercept).
#' These are useful for annotating plots. They are the labelled equivalent of
#' the `geom_vline`, `geom_hline` and `geom_abline` from ggplot2.
#'
#' Although reference lines are straight, and therefore don't lend themselves
#' to curved text, these geom layers are included in this package because they
#' make labelling reference lines easier, allow automatic line breaking if
#' desired, and will translate nicely into polar co-ordinates.
#'
#' These geoms act slightly differently from other geoms. You can supply the
#' parameters in two ways: either as arguments to the layer function,
#' or via aesthetics. If you use arguments, e.g.
#' `geom_textabline(label = "my label", intercept = 0, slope = 1)`, then behind
#' the scenes the geom makes a new data frame containing just the data you've
#' supplied. That means that the lines will be the same in all facets; if you
#' want them to vary across facets, construct the data frame yourself and use
#' aesthetics.
#'
#' Unlike most other geoms, these geoms do not inherit aesthetics from the plot
#' default, because they do not understand x and y aesthetics which are
#' commonly set in the plot. They also do not affect the x and y scales.
#'
#' @eval rd_dots(geom_textabline)
#' @param slope The slope of the abline
#' @param intercept the point on the y axis at which the abline crosses it.
#' @inheritParams geom_textpath
#' @seealso Other [geom layers][sibling_layers] that place text on paths.
#' @return A `Layer` ggproto object that can be added to a plot.
#' @export
#' @md
#' @eval rd_aesthetics("geom", "textabline")
#' @examples
#' ggplot(mtcars, aes(mpg, disp)) +
#' geom_point() +
#' geom_texthline(yintercept = 200, label = "displacement threshold",
#'                hjust = 0.8, color = "red4") +
#' geom_textvline(xintercept = 20, label = "consumption threshold", hjust = 0.8,
#'                linetype = 2, vjust = 1.3, color = "blue4") +
#' geom_textabline(slope = 15, intercept = -100, label = "partition line",
#'                 color = "green4", hjust = 0.6, vjust = -0.2)
geom_textabline <- function(mapping = NULL,
                            data = NULL,
                            slope,
                            intercept,
                            stat        = "identity",
                            position    = "identity",
                            ...,
                            arrow       = NULL,
                            lineend     = "butt",
                            na.rm       = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {

  # If nothing set, default to y = x
  if (is.null(mapping) && missing(slope) && missing(intercept)) {
    slope <- 1
    intercept <- 0
  }

  # Act like an annotation
  if (!missing(slope) || !missing(intercept)) {

    # Warn if supplied mapping and/or data is going to be overwritten
    if (!is.null(mapping)) {
      warn_overwritten_args("geom_textabline()", "mapping",
                            c("slope", "intercept"))
    }
    if (!is.null(data)) {
      warn_overwritten_args("geom_textabline()", "data",
                            c("slope", "intercept"))
    }

    if (missing(slope)) slope <- 1
    if (missing(intercept)) intercept <- 0

    if (is.null(data)) {
    data <- data_frame(
      intercept = intercept[1],
      slope = slope[1]
    )
    } else {
      data$slope <- slope[1]
      data$intercept <- intercept[1]
    }

    mapping <- unclass(mapping)
    mapping[["intercept"]] <- intercept
    mapping[["slope"]] <- slope
    class(mapping) <- "uneval"
    show.legend <- FALSE
  }

  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatIdentity,
    geom        = GeomTextabline,
    position    = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params      = list(
            na.rm   = na.rm,
            arrow   = arrow,
            lineend = lineend,
            ...
    )
  )
}

#' @rdname geom_textabline
#' @include geom_textsegment.R
#' @format NULL
#' @usage NULL
#' @export
GeomTextabline <- ggproto("GeomTextabline", GeomTextsegment,
  draw_panel = function(data, panel_params, coord, lineend = "butt",
                        arrow = arrow,
                        text_params = static_text_params("text")) {
    ranges <- coord$backtransform_range(panel_params)

    if (coord$clip == "on" && coord$is_linear()) {
      # Ensure the line extends well outside the panel to avoid visible line
      # ending for thick lines
      ranges$x <- ranges$x + c(-1, 1) * diff(ranges$x)
    }

    data$x    <- ranges$x[1]
    data$xend <- ranges$x[2]
    data$y    <- ranges$x[1] * data$slope + data$intercept
    data$yend <- ranges$x[2] * data$slope + data$intercept

    GeomTextsegment$draw_panel(unique(data), panel_params, coord,
                               lineend = lineend, text_params = text_params,
                               arrow = arrow)
  },

  required_aes = c("label", "slope", "intercept")
)

#' @rdname geom_textabline
#' @export
geom_labelabline <- function(mapping = NULL,
                            data = NULL,
                            slope,
                            intercept,
                            stat          = "identity",
                            position      = "identity",
                            ...,
                            arrow         = NULL,
                            lineend       = "butt",
                            straight      = NULL,
                            label.r       = unit(0.15, "lines"),
                            label.padding = unit(0.25, "lines"),
                            na.rm         = FALSE,
                            show.legend   = NA,
                            inherit.aes   = TRUE) {

  # If nothing set, default to y = x
  if (is.null(mapping) && missing(slope) && missing(intercept)) {
    slope <- 1
    intercept <- 0
  }

  # Act like an annotation
  if (!missing(slope) || !missing(intercept)) {

    # Warn if supplied mapping and/or data is going to be overwritten
    if (!is.null(mapping)) {
      warn_overwritten_args("geom_labelabline()", "mapping",
                            c("slope", "intercept"))
    }
    if (!is.null(data)) {
      warn_overwritten_args("geom_labelabline()", "data",
                            c("slope", "intercept"))
    }

    if (missing(slope)) slope <- 1
    if (missing(intercept)) intercept <- 0

    if (is.null(data)) {
    data <- data_frame(
      intercept = intercept[1],
      slope = slope[1]
    )
    } else {
      data$slope <- slope[1]
      data$intercept <- intercept[1]
    }

    mapping <- unclass(mapping)
    mapping[["intercept"]] <- intercept
    mapping[["slope"]] <- slope
    class(mapping) <- "uneval"
    show.legend <- FALSE
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomLabelabline,
    position = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list(
      arrow         = arrow,
      na.rm         = na.rm,
      lineend       = lineend,
      straight      = straight,
      label.r       = label.r,
      label.padding = label.padding,
      ...
    )
  )
}

#' @rdname geom_textabline
#' @format NULL
#' @usage NULL
#' @export
GeomLabelabline <- ggproto("GeomLabelabline", GeomLabelsegment,
  draw_panel = function(data, panel_params, coord, lineend = "butt",
                        arrow = arrow,
                        text_params = static_text_params("label"),
                        label.padding = unit(0.25, "lines"),
                        label.r = unit(0.15, "lines")) {
    ranges <- coord$backtransform_range(panel_params)

    if (coord$clip == "on" && coord$is_linear()) {
      # Ensure the line extends well outside the panel to avoid visible line
      # ending for thick lines
      ranges$x <- ranges$x + c(-1, 1) * diff(ranges$x)
    }

    data$x    <- ranges$x[1]
    data$xend <- ranges$x[2]
    data$y    <- ranges$x[1] * data$slope + data$intercept
    data$yend <- ranges$x[2] * data$slope + data$intercept

    GeomLabelsegment$draw_panel(unique(data), panel_params, coord,
                               lineend = lineend, text_params = text_params,
                               arrow = arrow,
                               label.padding = unit(0.25, "lines"),
                               label.r = unit(0.15, "lines"))
  },

  required_aes = c("label", "slope", "intercept")
)
