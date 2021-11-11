##---------------------------------------------------------------------------##
##                                                                           ##
##  geomtextpath main                                                        ##
##                                                                           ##
##  Copyright (C) 2021 by Allan Cameron                                      ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##


## -----------------------------------------------------------------------------
#' Add Curved Text Along Paths in \code{ggplot2}
#'
#' @description The existing text-based geom layers in ggplot2
#' (\code{geom_text} and \code{geom_label}) are ideal for the majority of plots,
#' since typically textual annotations are short, straight and in line with the
#' axes of the plot. However, there are some occasions when it is useful to have
#' text follow a curved path. This may be to create or recreate a specific
#' visual effect, or it may be to label a circular / polar plot in a more
#' "natural" way.
#'
#' There are limitations inherent in the plotting of text elements in
#' \code{ggplot} due to the way that the underlying \code{grid} graphics handles
#' text. A text string is dealt with as a zero-width object, and therefore the
#' rotation and spacing of the letters making up the string can only be dealt
#' with by treating each letter separately. Inevitably, this means
#' that curved text paths have to be calculated based on the size and aspect
#' ratio of the plotting device. Resizing the device after drawing a curved
#' text path will therefore cause artefacts of spacing and rotation in the text.
#'
#' It is important to realise that the letters are only rotated, and do not
#' undergo any change in shape. Thus, for example, large text appearing on
#' convex curves will not be deformed so that individual letters are narrower at
#' the bottom and wider at the top. Doing so would require reinterpreting the
#' letters as polygons.
#'
#' Another issue is that we may wish to use a short curved label on a much
#' longer path. Spacing the letters equally along the path would mean there is
#' too much space between the letters for the label to remain legible. A single
#' text string is therefore kept "together" according to the point size of the
#' text in \code{geom_textpath}. This then leaves the problem of where on the
#' path the text should be placed. This can be dealt with by the aesthetic
#' mapping \code{start_proportion}, which allows the user to place the labels
#' at the desired position along the path, including separate positions for
#' each label.
#'
#' A final point to note is that a path is usually a group-based geom (i.e.
#' a path typically comprises x, y points from two columns over several rows of
#' a data frame), whereas text labels can come from single rows in a data frame.
#' This means that if we have a data frame with an x column, a y column and a
#' grouping variable column, there can only be a single label for the group.
#' Typically, this will be the grouping variable itself (see the examples,
#' particularly those using the built-in \code{iris} data set.)
#'
#'
#' @inheritParams ggplot2::layer
#' @param ... other arguments passed on to \code{\link{layer}}. These are often
#' aesthetics, used to set an aesthetic to a fixed value, like \code{color =
#' "red"} or \code{size = 3}. They may also be parameters to the paired
#'  geom/stat.
#' @param start_proportion how far along the path should the text start?
#' This is useful for situations with longer paths and shorter strings.
#' @param na.rm Removes missing points or labels from the text path.
#'
#' @section Aesthetics:
#' \code{geom_textpath()} understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \strong{\code{label}}
#'   \item \code{alpha}
#'   \item \code{colour}
#'   \item \code{family}
#'   \item \code{fontface}
#'   \item \code{group}
#'   \item \code{hjust}
#'   \item \code{size}
#'   \item \code{vjust}
#' }
#'
#' @export
#'
#' @examples
#'
#' # Plot text along a spiral path
#'
#' spiral <- data.frame(x = rev(sin(seq(0, 5*pi, length.out = 1000)) * 1000:1),
#'                      y = rev(cos(seq(0, 5*pi, length.out = 1000)) * 1000:1),
#'                      s = seq(1, 10, length.out = 1000),
#'                      z = paste("Like a circle in a spiral, like a",
#'                                "wheel within a wheel, never ending",
#'                                "or beginning on an ever spinning reel"))
#'
#' ggplot(spiral, aes(x, y, label = z)) +
#'   geom_textpath(start_proportion = 0.2, size = 5.9, vjust=-0.4) +
#'   geom_path(color = "gray90") +
#'   coord_equal() +
#'   theme_void()
#'
#'
#' # Straight text paths in Cartesian Co-ordinates curve in Polar Co-ordinates
#'
#' df <- data.frame(x = 1:1000, y = 1, z = "This is a perfectly flat label")
#'
#' p <- ggplot(df, aes(x, y, label = z)) +
#'      geom_textpath(start_proportion = 0.25, size = 6)
#'
#' p
#'
#' p + coord_polar()
#'
#'
#' # Plot labels within groups of points using stat = "smooth"
#'
#' ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
#'   geom_point(alpha = 0.5) +
#'   geom_textpath(aes(label = Species,
#'                 color = stage(Species, after_scale = darken(color, 1.5))),
#'                 size = 5, stat = "smooth",
#'                 start_proportion = 0.25, fontface = 2)
#'
#'
#' # Plot labels along density curves using stat = "density"
#'
#' ggplot(iris, aes(x = Sepal.Length, color = Species)) +
#'   geom_density(alpha = 0.5) +
#'   geom_textpath(aes(label = as.character(Species),
#'                     color = stage(Species, after_scale = darken(color, 1.5)),
#'                     vjust = -0.1),
#'                 size = 5, stat = "density",
#'                 start_proportion = 0.12, fontface = 2)
#'

geom_textpath <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE,
                          start_proportion = 0.45, ...)
{
  ggplot2::layer(geom = GeomTextPath, mapping = mapping, data = data, stat = stat,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(start_proportion = start_proportion, ...))
}

## -----------------------------------------------------------------------------

.add_path_data <- function(.data)
{
  grad <- diff(.data$y) / diff(.data$x)
  .data$grad <- c(grad[1], grad)

  .data$angle <- atan(.data$grad) * 180 / pi
  .data$angle <- ifelse(sign(c(0, diff(.data$x))) < 0,
                        .data$angle - 180, .data$angle)
  .data$length <- c(0, cumsum(sqrt(diff(.data$x)^2 + diff(.data$y)^2)))
  .data$string_length <- sapply(.data$label, strwidth, units = "figure")

  .data
}

## -----------------------------------------------------------------------------
.get_path_points <- function(path, x_start)
{
  n       <- nrow(path)
  x_start <- round(n * x_start)
  x_start <- if(x_start < 1) path$x[1] else path$x[x_start]


  letters      <- strsplit(path$label[1], "")[[1]]
  letterwidths <- cumsum(strwidth(letters, units = "figure"))

  start_dist  <- approx(x = path$x, y = path$length, xout = x_start,
                        ties = mean)$y
  diff_dist   <- path$size[1] * 0.5
  dist_points <- c(start_dist, letterwidths * diff_dist + start_dist)
  dist_points <- (head(dist_points, -1) + tail(dist_points, -1)) / 2

  df <- as.data.frame(lapply(path, function(i) {
    if(is.numeric(i))
      approx(x = path$length, y = i, xout = dist_points, ties = mean)$y
    else
      rep(i[1], length(dist_points))
    }))
  df$label <- letters
  df[!is.na(df$angle), ]
}

## -----------------------------------------------------------------------------

#' The Geom object for a textpath
#'
#' This is the \code{ggproto} class that creates the textpath layer. It is not
#' intended to be used directly by the end user.
#'
#' @format NULL
#' @usage NULL
#' @export

GeomTextPath <- ggplot2::ggproto("GeomTextPath", ggplot2::Geom,
  required_aes = c("x", "y", "label"),
  default_aes = ggplot2::aes(colour = "black", size = 3.88, hjust = 0.5, vjust = 0.5,
                    family = "", fontface = 1, lineheight = 1.2, alpha = NA),
  extra_params = c("na.rm", "start_proportion"),
  draw_key = ggplot2::draw_key_text,
  draw_panel = function(data, panel_params, coord, start_proportion = 0.45) {

    if(is.factor(data$label)) data$label <- as.character(data$label)

    data$group <- as.numeric(factor(data$label))

    max_str <- max(strwidth(data$label, units = "figure"))

    if (!anyDuplicated(data$group)) {
        message_wrap("geom_path: Each group consists of only one observation. ",
            "Do you need to adjust the group aesthetic?")
    }

    if(!all(sapply(split(data, data$group),
           function(x) all(x$label == x$label[1]))))
    {
      warning("Only the first label in each group will be used")
    }


    data <- data[order(data$group), , drop = FALSE]

    if(start_proportion < 0 | start_proportion > 1)
    {
      warning("start_proportion must be between 0 and 1. Defaulting to 0.45")
      start_proportion <- 0.45
    }

    data <- coord$transform(data, panel_params)
    data <- do.call(rbind, lapply(split(data, data$group), .add_path_data))
    data <- do.call(rbind, lapply(split(data, data$group), function(d) {
      .get_path_points(d, start_proportion)
    }))

    my_tree <- grid::gTree()

    for(i in seq(nrow(data))) {
      my_tree <- grid::addGrob(my_tree, grid::textGrob(
        label = data$label[i],
        x = data$x[i],
        y = data$y[i],
        hjust = data$hjust[i],
        vjust = data$vjust[i],
        rot = data$angle[i],
        default.units = "native",
        gp = grid::gpar(
          col = scales::alpha(data$colour[i], data$alpha[i]),
          fontsize = data$size[i] * 4,
          fontface = data$fontface[i],
          fontfamily = data$fontfamily[i])))
    }

    return(my_tree)
  }
)
