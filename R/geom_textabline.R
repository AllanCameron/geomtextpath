##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_textabline.R                                                        ##
##  Part of the geomtextpath R package                                       ##
##                                                                           ##
##  Copyright (C) 2021 - 2022 by Allan Cameron & Teun van den Brand          ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

# Constructors -----------------------------------------------------------------

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
#' @section Aesthetics:
#' The `geom_textabline()`, `geom_texthline()` and `geom_textvline()` understand
#' the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item{\strong{`label`}}
#'   \item{\strong{`slope`} (`geom_textabline()` and `geom_labelabline()`)}
#'   \item{\strong{`intercept`} (`geom_textabline()` and
#'     `geom_labelabline()`)}
#'   \item{\strong{`yintercept`} (`geom_texthline()` and
#'     `geom_labelhline()`)}
#'   \item{\strong{`xintercept`} (`geom_textvline()` and
#'     `geom_labelvline()`)}
#'   \item{`alpha`}
#'   \item{`angle`}
#'   \item{`colour`}
#'   \item{`family`}
#'   \item{`fontface`}
#'   \item{`group`}
#'   \item{`hjust`}
#'   \item{`vjust`}
#'   \item{`linecolour`}
#'   \item{`lineheight`}
#'   \item{`linetype`}
#'   \item{`linewidth`}
#'   \item{`size`}
#'   \item{`spacing`}
#'   \item{`textcolour`}
#' }
#' In addition to aforementioned aesthetics, `geom_labelabline()`,
#' `geom_labelhline()` and `geom_labelvline()` also understand:
#' \itemize{
#'   \item{`boxcolour`}
#'   \item{`boxlinetype`}
#'   \item{`boxlinewidth`}
#'   \item{`fill`}
#' }
#' The `spacing` aesthetic allows fine control of spacing of text, which is
#' called 'tracking' in typography. The default is 0 and units are measured in
#' 1/1000 em. Numbers greater than zero increase the spacing, whereas negative
#' numbers decrease the spacing.
#'
#' @examples
#' ggplot(mtcars, aes(mpg, disp)) +
#' geom_point() +
#' geom_texthline(yintercept = 200, label = "displacement threshold",
#'                hjust = 0.8, color = "red4") +
#' geom_textvline(xintercept = 20, label = "consumption threshold", hjust = 0.8,
#'                linetype = 2, vjust = 1.3, color = "blue4") +
#' geom_textabline(slope = 15, intercept = -100, label = "partition line",
#'                 color = "green4", hjust = 0.6, vjust = -0.2)
geom_textabline <- function(
  mapping     = NULL,
  data        = NULL,
  slope,
  intercept,
  ...,
  na.rm       = FALSE,
  show.legend = NA
) {

  construct_abline(
    mapping     = mapping,
    data        = data,
    slope       = slope,
    intercept   = intercept,
    show.legend = show.legend,
    na.rm       = na.rm,
    ...,
    layername   = "geom_textabline()",
    super       = GeomTextabline
  )
}


#' @rdname geom_textabline
#' @export
geom_labelabline <- function(
  mapping       = NULL,
  data          = NULL,
  slope,
  intercept,
  ...,
  straight      = NULL,
  label.r       = unit(0.15, "lines"),
  label.padding = unit(0.25, "lines"),
  na.rm         = FALSE,
  show.legend   = NA
) {

  construct_abline(
    mapping       = mapping,
    data          = data,
    slope         = slope,
    intercept     = intercept,
    show.legend   = show.legend,
    na.rm         = na.rm,
    ...,
    label.r       = label.r,
    label.padding = label.padding,
    layername     = "geom_labelabline()",
    super         = GeomLabelabline
  )
}


# ggproto classes ---------------------------------------------------------

#' @rdname GeomTextpath
#' @include geom_textsegment.R
#' @format NULL
#' @usage NULL
#' @export
GeomTextabline <- ggproto("GeomTextabline", GeomTextsegment,

  draw_panel = function(data, panel_params, coord, lineend = "butt",
                        arrow = NULL,
                        text_params = static_text_params("text")) {
    abline2path(data, panel_params, coord, super = GeomTextsegment,
                lineend = lineend, arrow = arrow, text_params = text_params)
  },

  required_aes = c("label", "slope", "intercept")
)


#' @rdname GeomTextpath
#' @format NULL
#' @usage NULL
#' @export
GeomLabelabline <- ggproto("GeomLabelabline", GeomLabelsegment,

  draw_panel = function(data, panel_params, coord, lineend = "butt",
                        arrow = NULL,
                        text_params = static_text_params("label"),
                        label.padding = unit(0.25, "lines"),
                        label.r = unit(0.15, "lines")) {
    abline2path(data, panel_params, coord, super = GeomLabelsegment,
                lineend = lineend, arrow = arrow, text_params = text_params,
                label.padding = label.padding, label.r = label.r)
  },

  required_aes = c("label", "slope", "intercept")
)


# Helpers ----------------------------------------------------------------------

construct_abline <- function(
  mapping     = NULL,
  data        = NULL,
  slope,
  intercept,
  ...,
  show.legend = NA,
  layername   = "geom_textabline()",
  super       = GeomTextabline
) {

  # If nothing set, default to y = x
  if (is.null(mapping) && missing(slope) && missing(intercept)) {
    slope     <- 1
    intercept <- 0
  }

  if (!missing(slope) || !missing(intercept)) {

    if (!is.null(mapping)) {
      warn_overwritten_args(layername, "mapping", c("slope", "intercept"))
    }
    if (!is.null(data)) {
      warn_overwritten_args(layername, "data", c("slope", "intercept"))
    }

    if (missing(slope))     slope <- 1
    if (missing(intercept)) intercept <- 0

    if (is.null(data)) {
      data <- data_frame(
        intercept = intercept[1],
        slope     = slope[1]
      )
    } else {
      data$slope     <- slope[1]
      data$intercept <- intercept[1]
    }

    mapping <- unclass(mapping)
    mapping[["intercept"]] <- intercept
    mapping[["slope"]]     <- slope
    class(mapping) <- "uneval"
    show.legend <- FALSE
  }

  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatIdentity,
    position    = PositionIdentity,
    geom        = super,
    inherit.aes = FALSE,
    params      = set_params(
      .type = if (inherits(super, "GeomLabelpath")) "label" else "text",
      ...
    )
  )
}


abline2path <- function(
  data,
  panel_params,
  coord,
  ...,
  super = GeomTextsegment
) {

  ranges <- coord$backtransform_range(panel_params)
  ranges$xorig <- ranges$x

  if (coord$clip == "on" && coord$is_linear()) {
    # Ensure the line extends well outside the panel to avoid visible line
    # ending for thick lines
    ranges$x <- ranges$x + c(-1, 1) * diff(ranges$x)
  }

  data$x    <- ranges$x[1]
  data$xend <- ranges$x[2]
  data$y    <- ranges$x[1] * data$slope + data$intercept
  data$yend <- ranges$x[2] * data$slope + data$intercept

  # Calculated bounded start and endpoints
  left_x  <- (ranges$y[1] - data$intercept) / data$slope
  right_x <- (ranges$y[2] - data$intercept) / data$slope
  xmin <- pmax(pmin(left_x, right_x), ranges$xorig[1])
  xmax <- pmin(pmax(left_x, right_x), ranges$xorig[2])
  ymin <- xmin * data$slope + data$intercept
  ymax <- xmax * data$slope + data$intercept

  # Calculate start and end of visual proportion of the line
  len <- sqrt((data$xend - data$x)^2 + (data$yend - data$y)^2) # total length
  justmin <- sqrt((xmin - data$x)^2 + (ymin - data$y)^2) / len # start
  justmax <- sqrt((xmax - data$x)^2 + (ymax - data$y)^2) / len # end

  # Rescale hjust accordingly
  if (is.numeric(data$hjust)) {
    data$hjust <- data$hjust * (justmax - justmin) + justmin
  }

  super$draw_panel(unique(data), panel_params, coord, ...)
}
