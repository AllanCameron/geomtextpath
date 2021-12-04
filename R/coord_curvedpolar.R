##---------------------------------------------------------------------------##
##                                                                           ##
##  geom_textpath coord_curvedpolar                                          ##
##                                                                           ##
##  Copyright (C) 2021 by Allan Cameron & Teun van den Brand                 ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

#' Polar coordinates with curved text on x axis
#'
#' Polar co-ordinates in `ggplot2` help to create a range of circular plots,
#' which can be used to present data in a visually appealing, user-friendly way.
#' However, the standard `coord_polar` uses a `textGrob` to render the labels
#' on the circumferential (theta) axis, meaning that labels do not rotate or
#' curve in line with the axis. `coord_curvedpolar` aims to be identical to
#' `coord_polar`, except that the text on the theta axis follows the curve
#' of the plot, correcting automatically for resizing to preserve letter spacing
#' and size.
#'
#'
#' @param theta variable to map angle to (`x` or `y`)
#' @param start Offset of starting point from 12 o'clock in radians. Offset
#'   is applied clockwise or anticlockwise depending on value of `direction`.
#' @param direction 1, clockwise; -1, anticlockwise
#' @param clip Should drawing be clipped to the extent of the plot panel? A
#'   setting of `"on"` (the default) means yes, and a setting of `"off"`
#'   means no. For details, please see [`coord_cartesian()`].
#' @param halign Determines the alignment for multi-line text labels
#' @export
#' @examples
#'
#' # A pie chart = stacked bar chart + polar coordinates
#' pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
#'  geom_bar(width = 1)
#'  pie + coord_curvedpolar(theta = "y")
#'
#' # Demonstrating curved category labels
#' p <- ggplot(data.frame(x = paste("Category label", 1:5), y = runif(5)),
#'             aes(x, y, fill = x)) +
#'        geom_col() +
#'        theme_bw() +
#'        theme(panel.border = element_blank(),
#'              legend.position = "none",
#'              axis.text.x = element_text(size = 10, vjust = 0.5))
#'
#' # Standard bar chart in Cartesian Co-ordinates
#' p
#'
#' # Standard coord_polar axis labels
#' p + coord_polar()
#'
#' # Curved polar co-ordinate labels
#' p + coord_curvedpolar()
#'

coord_curvedpolar <- function(theta = "x", start = 0,
                              direction = 1, clip = "on",
                              halign = c("center")) {

  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"

  ggproto(NULL, CoordPolar,
    theta = theta,
    r = r,
    start = start,
    direction = sign(direction),
    clip = clip,
    halign = halign,

    render_fg = function (self, panel_params, theme) {

    if (is.null(panel_params$theta.major)) {
        return(element_render(theme, "panel.border"))
    }
    txt_el <- calc_element("axis.text.x", theme)

    # Early exit if text is blank
    if (inherits(txt_el, "element_blank")) {
      out <- grobTree(zeroGrob(), element_render(theme, "panel.border"))
      return(out)
    }


    theta <- .theta_rescale(self, panel_params$theta.major, panel_params)
    labels <- panel_params$theta.labels
    theta <- theta[!is.na(theta)]
    ends_apart <- (theta[length(theta)] - theta[1])%%(2 * pi)

    if (length(theta) > 0 && ends_apart < 0.05) {
        n <- length(labels)
        if (is.expression(labels)) {
            combined <- substitute(paste(a, "/", b), list(a = labels[[1]],
                b = labels[[n]]))
        }
        else {
            combined <- paste(labels[1], labels[n], sep = "/")
        }
        labels[[n]] <- combined
        labels <- labels[-1]
        theta <- theta[-1]
    }

    # Rather than rendering a bespoke element_textpath via element_grob,
    # we harvest the appropriate parameters using calc_element directly

    element_gp <- gpar(fontsize = rep(txt_el$size, length(labels)),
                       col = rep(txt_el$colour, length(labels)),
                       fontfamily = txt_el$family,
                       fontface = txt_el$face,
                       lineheight = txt_el$lineheight)

    path_gp <- gpar(col = "black", fill = "black", lwd = 1, lty  = 1)

    # This constructs a circular path for the labels to sit on.
    wid <- mean(diff(theta))

    path_t <- seq(-wid/2, wid/2, len = 1000)

    id <- rep(seq_along(labels), each = length(path_t))

    theta <- as.vector(t(outer(theta, path_t, "+")))
    x <- 0.45 * sin(theta) + 0.5
    y <- 0.45 * cos(theta) + 0.5

    # We now have enough data to make our grob
    grid::grobTree(if (length(labels) > 0)
      textpathGrob(labels,
                    x = x,
                    y = y,
                    id = id,
                    hjust = txt_el$hjust,
                    vjust = txt_el$vjust,
                    halign = halign,
                    gp_text = element_gp,
                    gp_path = gpar(),
                    flip_inverted = TRUE,
                    default.units = "native"
                  ),
      element_render(theme, "panel.border")
       )
    }
  )
}


# A straight reimplementation of ggplot2:::theta_rescale to avoid using
# non-exported functions

.theta_rescale <- function (coord, x, panel_params)
{
    x <- scales::squish_infinite(x, panel_params$theta.range)
    rotate <- function(x) (x + coord$start)%%(2 * pi) * coord$direction
    rotate(scales::rescale(x, c(0, 2 * pi), panel_params$theta.range))
}
