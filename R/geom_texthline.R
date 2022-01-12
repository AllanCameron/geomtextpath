#' @rdname geom_textabline
#' @eval rd_dots(geom_texthline)
#' @param yintercept The value at which the line should intercept the y axis
#' @export
#' @md

geom_texthline <- function(mapping = NULL,
                           data = NULL,
                           yintercept,
                           stat = "identity",
                           position = "identity",
                           ...,
                           arrow = NULL,
                           lineend = "butt",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
  # Act like an annotation
  if (!missing(yintercept)) {
    # Warn if supplied mapping and/or data is going to be overwritten
    if (!is.null(mapping)) {
      warn_overwritten_args("geom_texthline()", "mapping", "yintercept")
    }
    if (!is.null(data)) {
      warn_overwritten_args("geom_texthline()", "data", "yintercept")
      data$yintercept <- yintercept
    }
    mapping <- unclass(mapping)
    mapping[["yintercept"]] <- yintercept
    class(mapping) <- "uneval"

    mapping <- aes(yintercept = yintercept)
    show.legend <- FALSE
  }

  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatIdentity,
    geom        = GeomTexthline,
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
GeomTexthline <- ggproto("GeomTexthline", GeomTextpath,
  draw_panel = function(data, panel_params, coord, lineend = "butt",
                        arrow = arrow,
                        text_params = static_text_params("label")) {
    ranges <- coord$backtransform_range(panel_params)

    data$x    <- ranges$x[1]
    data$xend <- ranges$x[2]
    data$y    <- data$yintercept
    data$yend <- data$yintercept

    GeomTextsegment$draw_panel(unique(data), panel_params,
                               coord, lineend = lineend,
                               text_params = text_params, arrow = arrow)
  },

  required_aes = c("yintercept", "label")
)

#' @rdname geom_textabline
#' @export
geom_labelhline <- function(mapping = NULL,
                            data = NULL,
                            yintercept,
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
  if (!missing(yintercept)) {
    # Warn if supplied mapping and/or data is going to be overwritten
    if (!is.null(mapping)) {
      warn_overwritten_args("geom_labelhline()", "mapping", "yintercept")
    }
    if (!is.null(data)) {
      warn_overwritten_args("geom_labelhline()", "data", "yintercept")
      data$yintercept <- yintercept
    }
    mapping <- unclass(mapping)
    mapping[["yintercept"]] <- yintercept
    class(mapping) <- "uneval"

    mapping <- aes(yintercept = yintercept)
    show.legend <- FALSE
  }

  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatIdentity,
    geom        = GeomLabelhline,
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
GeomLabelhline <- ggproto("GeomLabelhline", GeomLabelpath,
  draw_panel = function(data, panel_params, coord, lineend = "butt",
                        text_params = static_text_params("label"), arrow = NULL,
                        label.padding = unit(0.25, "lines"),
                        label.r = unit(0.15, "lines")
  ) {
    ranges <- coord$backtransform_range(panel_params)

    data$x    <- ranges$x[1]
    data$xend <- ranges$x[2]
    data$y    <- data$yintercept
    data$yend <- data$yintercept

    GeomLabelsegment$draw_panel(unique(data), panel_params,
                               coord, lineend = lineend,
                               text_params = text_params, arrow = arrow,
                               label.padding = label.padding,
                               label.r = label.r)
  },

  required_aes = c("yintercept", "label")
)
