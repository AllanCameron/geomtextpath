
#' Textpath axis
#'
#' This is a specialised guide used in \code{coord_radial()} to represent
#' the theta position scale.
#'
#' @inheritParams ggplot2::guide_axis_theta
#' @inheritParams geom_textpath
#'
#' @return A \code{Guide} ggproto object that can be added to a plot through a
#'  scale or the \code{guides()} function.
#' @export
#'
#' @examples
#' ggplot(mpg, aes(class, displ)) +
#'   geom_boxplot(staplewidth = 0.5) +
#'   coord_radial() +
#'   guides(theta = "axis_textpath")
guide_axis_textpath <- function(title = waiver(), halign = "center",
                                rich = FALSE, minor.ticks = FALSE,
                                cap = "none", order = 0, position = waiver()) {

  if (!(base::isTRUE(minor.ticks) || base::isFALSE(minor.ticks))) {
    abort("`minor.ticks` must be either `TRUE` or `FALSE.")
  }
  if (is.logical(cap)) {
    if (!(base::isTRUE(cap) || base::isFALSE(cap))) {
      abort("`cap` must be either `TRUE`, `FALSE` or a <character>.")
    }
    cap <- if (cap) "both" else "none"
  }

  cap <- rlang::arg_match0(cap, c("none", "both", "upper", "lower"))

  new_guide(
    title = title,
    halign = halign,
    rich = rich,
    cap = cap,
    minor.ticks = minor.ticks,
    available_aes = c("x", "y", "theta"),
    order = order,
    position = position,
    name = "axis",
    super = GuideAxisTextpath
  )
}

#' @rdname GeomTextpath
#' @export
#' @format NULL
#' @usage NULL
GuideAxisTextpath <- ggproto(
  NULL, GuideAxisTheta,

  params = c(
    GuideAxisTheta$params,
    list(halign = "center", rich = FALSE)
  ),

  transform = function(self, params, coord, panel_params) {
    params$polar_params <- get_polar_params(coord, panel_params)
    GuideAxisTheta$transform(params, coord, panel_params)
  },

  build_labels = function(key, elements, params) {
    if (inherits(elements$text, "element_blank")) {
      return(zeroGrob())
    }

    key <- key[!detect_missing(key, ".label"), , drop = FALSE]
    labels <- key$.label
    theta  <- key$theta
    if (length(labels) < 1) {
      return(zeroGrob())
    }

    offset <- elements$offset
    if (!is.null(params$stack_offset)) {
      offset <- offset + params$stack_offset
    }
    xoffset <- offset * sin(theta)
    yoffset <- offset * cos(theta)

    elem <- elements$text
    elem[names(elem) != "margin"] <- lapply(
      elem[names(elem) != "margin"],
      rep, length.out = length(labels)
    )

    vjust <- switch(
      params$position,
      theta = 1, theta.sec = 0,
      elem$vjust
    )

    textpathGrob(
      labels,
      x = unit(key$x, "npc") + xoffset,
      y = unit(key$y, "npc") + yoffset,
      id = seq_len(nrow(key)),
      hjust = elem$hjust,
      vjust = vjust,
      gp_text = gpar(
        col        = elem$colour,
        fontsize   = elem$size,
        fontface   = elem$fontface %||% elem$font,
        fontfamily = elem$family,
        lineheight = elem$lineheight
      ),
      gp_path = gpar(linetype = 0, lty = 0),
      rich = params$rich,
      upright = TRUE,
      polar_params = params$polar_params
    )
  },

  measure_grobs = function(grobs, params, elements) {
    if (is.null(params$stack_offset)) {
      return(NULL)
    }
    # When this axis is part of a stack of axes we need to know the height
    # of the text
    labels <- grobs$labels$textpath$label
    if (length(labels) < 1 || inherits(labels, "zeroGrob")) {
      return(elements$offset)
    }
    heights <- numapply(labels, function(x) attr(x, "metrics")$height)
    list(offset = unit(max(heights), "in") + elements$offset)
  }
)
