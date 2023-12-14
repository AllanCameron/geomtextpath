
#' Textpath axis
#'
#' This is a specialised guide used in \code{coord_radial()} to represent
#' the theta position scale.
#'
#' @inheritParams guide_axis_theta
#' @inheritParams geom_textpath
#'
#' @return A \code{Guide} ggproto object that can be added to a plot through a
#'  scale or the \code{guides()} function.
#' @export
#'
#' @examples
guide_axis_textpath <- function(title = waiver(), halign = "center",
                                rich = FALSE, minor.ticks = FALSE,
                                cap = "none", order = 0, position = waiver()) {

  if (!(isTRUE(minor.ticks) || isFALSE(minor.ticks))) {
    abort("`minor.ticks` must be either `TRUE` or `FALSE.")
  }
  if (is.logical(cap)) {
    if (!(isTRUE(cap) || isFALSE(cap))) {
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

#' The Guide object for an axis textpath.
#'
#' This is the \code{ggproto} class that creates the axis textpath. It is not
#' intended t o be used directly by the end user.
#'
#' @export
#' @format NULL
#' @usage NULL
GuideAxisTextpath <- ggproto(
  NULL, ggplot2:::GuideAxisTheta,

  params = c(
    ggplot2:::GuideAxisTheta$params,
    list(halign = "center", rich = FALSE)
  ),

  transform = function(self, params, coord, panel_params) {
    params$polar_params <- get_polar_params(coord, panel_params)
    ggplot2:::GuideAxisTheta$transform(params, coord, panel_params)
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
    textpathGrob(
      labels,
      x = unit(key$x, "npc") + xoffset,
      y = unit(key$y, "npc") + yoffset,
      id = seq_len(nrow(key)),
      hjust = elem$hjust,
      vjust = elem$vjust,
      gp_text = gpar(
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
    labels <- grobs$labels$textpath$label
    if (length(labels) < 1) {
      return(elements$offset)
    }
    heights <- numapply(labels, function(x) attr(x, "metrics")$height)
    list(offset = unit(max(heights), "in") + elements$offset)
  }
)
