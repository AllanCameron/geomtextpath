#' @keywords internal
"_PACKAGE"

#' @import ggplot2
#' @import grid
#' @importFrom stats approx dnorm approxfun complete.cases ave bw.nrd setNames
#'   spline smooth.spline predict
#' @importFrom utils head tail
#' @importFrom graphics strwidth
#' @importFrom systemfonts glyph_info
#' @importFrom grDevices dev.size png
#' @importFrom rlang abort warn `%||%` `:=`
#' @importFrom scales rescale squish_infinite alpha
#' @importFrom textshaping shape_text
NULL

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

#' The goal of \pkg{geomtextpath} is to label (curved) lines in a plot. The
#' \pkg{ggplot2} package has various ways to construct lines. For several of
#' the \pkg{ggplot2} line functions, there is a plain 'text' sibling and a
#' 'label' sibling that includes a text box. Below is an overview of how
#' function in \pkg{geomtextpath} relate to those in \pkg{ggplot2}.
#'
#' | \pkg{ggplot2} geom | Text equivalent | Label equivalent |
#' |:--- |:--- |:--- |
#' | [`geom_path()`][ggplot2::geom_path] | [`geom_textpath()`] | [`geom_labelpath()`] |
#' | [`geom_line()`][ggplot2::geom_line] | [`geom_textline()`] | [`geom_labelline()`] |
#' | [`geom_segment()`][ggplot2::geom_segment] | [`geom_textsegment()`] | [`geom_labelsegment()`] |
#' | [`geom_curve()`][ggplot2::geom_curve] | [`geom_textcurve`] | [`geom_labelcurve()`] |
#' | [`geom_abline()`][ggplot2::geom_abline] | [`geom_textabline()`] | [`geom_labelabline()`] |
#' | [`geom_hline()`][ggplot2::geom_hline] | [`geom_texthline()`] | [`geom_labelhline()`] |
#' | [`geom_vline()`][ggplot2::geom_vline] | [`geom_textvline()`] | [`geom_labelvline()`] |
#' | [`geom_density()`][ggplot2::geom_density] | [`geom_textdensity()`] | [`geom_labeldensity()`] |
#' | [`geom_smooth()`][ggplot2::geom_smooth] | [`geom_textsmooth()`] | [`geom_labelsmooth()`] |
#' | [`geom_contour()`][ggplot2::geom_contour] | [`geom_textcontour()`] | [`geom_labelcontour()`] |
#' | [`geom_density2d()`][ggplot2::geom_density2d] | [`geom_textdensity2d()`] | [`geom_labeldensity2d()`] |
#' | [`geom_sf()`][ggplot2::geom_sf] | [`geom_textsf()`] | [`geom_labelsf()`] |
#'
#'
#' @name sibling_layers
#' @title Sibling layers
#' @md
NULL
