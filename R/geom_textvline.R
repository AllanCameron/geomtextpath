#' @rdname geom_textabline
#' @eval rd_dots(geom_textvline)
#' @param xintercept The value at which the line should intercept the y axis
#' @inheritParams geom_textpath
#' @export
#' @md
#' @eval rd_aesthetics("geom", "textvline")

geom_textvline <- function(mapping = NULL,
                           data = NULL,
                           xintercept,
                           stat = "identity",
                           position = "identity",
                           ...,
                           arrow = NULL,
                           lineend = "butt",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  # Act like an annotation
  if (!missing(xintercept)) {
    # Warn if supplied mapping and/or data is going to be overwritten
    if (!is.null(mapping)) {
      warn_overwritten_args("geom_textvline()", "mapping", "xintercept")
    }
    if (!is.null(data)) {
      warn_overwritten_args("geom_textvline()", "data", "xintercept")
      data$xintercept <- xintercept
    }
    mapping <- unclass(mapping)
    mapping[["xintercept"]] <- xintercept
    class(mapping) <- "uneval"

    mapping <- aes(xintercept = xintercept)
    show.legend <- FALSE
  }

  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatIdentity,
    geom        = GeomTextvline,
    position    = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list(
      na.rm = na.rm,
      arrow = arrow,
      lineend = lineend,
      ...
    )
  )
}

#' @rdname geom_textabline
#' @format NULL
#' @usage NULL
#' @export
GeomTextvline <- ggproto("GeomTextvline", GeomTextpath,
  draw_panel = function(data, panel_params, coord, lineend = "butt",
                        arrow = arrow,
                        text_params = static_text_params("label")) {
    ranges <- coord$backtransform_range(panel_params)

    data$x    <- data$xintercept
    data$xend <- data$xintercept
    data$y    <- ranges$y[1]
    data$yend <- ranges$y[2]

    GeomTextsegment$draw_panel(unique(data), panel_params,
                               coord, lineend = lineend,
                               arrow = arrow,
                               text_params = text_params)
  },

  required_aes = c("xintercept", "label")
)

#' @rdname geom_textabline
#' @export
geom_labelvline <- function(mapping = NULL,
                            data = NULL,
                            xintercept,
                            stat = "identity",
                            position = "identity",
                            ...,
                            arrow    = NULL,
                            lineend  = "butt",
                            na.rm    = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE,
                            straight    = NULL,
                            label.r     = unit(0.15, "lines"),
                            label.padding = unit(0.25, "lines")
                          ) {
  # Act like an annotation
  if (!missing(xintercept)) {
    # Warn if supplied mapping and/or data is going to be overwritten
    if (!is.null(mapping)) {
      warn_overwritten_args("geom_labelvline()", "mapping", "xintercept")
    }
    if (!is.null(data)) {
      warn_overwritten_args("geom_labelvline()", "data", "xintercept")
      data$xintercept <- xintercept
    }
    mapping <- unclass(mapping)
    mapping[["xintercept"]] <- xintercept
    class(mapping) <- "uneval"

    mapping <- aes(xintercept = xintercept)
    show.legend <- FALSE
  }

  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatIdentity,
    geom        = GeomLabelvline,
    position    = PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = set_params(
      arrow     = arrow,
      na.rm     = na.rm,
      lineend   = lineend,
      straight  = straight,
      label.r   = label.r,
      label.padding = label.padding,
      ...
    )
  )
}

#' @rdname geom_textabline
#' @format NULL
#' @usage NULL
#' @export
GeomLabelvline <- ggproto("GeomLabelvline", GeomLabelpath,
  draw_panel = function(data, panel_params, coord, lineend = "butt",
                        text_params = static_text_params("label"), arrow = NULL,
                        label.padding = unit(0.25, "lines"),
                        label.r = unit(0.15, "lines")
  ) {
    ranges <- coord$backtransform_range(panel_params)

    data$x    <- data$xintercept
    data$xend <- data$xintercept
    data$y    <- ranges$y[1]
    data$yend <- ranges$y[2]

    GeomLabelsegment$draw_panel(unique(data), panel_params,
                               coord, lineend = lineend,
                               text_params = text_params,
                               label.padding = label.padding,
                               label.r = label.r)
  },

  required_aes = c("xintercept", "label")
)
