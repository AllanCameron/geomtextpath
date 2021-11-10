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


#' Create text along a path
#' @description Allows text to follow a curved path in ggplot
#' @inheritParams ggplot2::layer
#' @param ... other arguments passed on to \code{\link{layer}}. These are often
#' aesthetics, used to set an aesthetic to a fixed value, like \code{color =
#' "red"} or \code{size = 3}. They may also be parameters to the paired
#'  geom/stat.
#' @param start_proportion how far along the path should the text start?
#' This is useful for situations with longer paths and shorter strings.
#' @export
#'
#'
#' @examples
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
#' df <- data.frame(x = 1:1000, y = 1, z = "This is a perfectly flat label")
#'
#' p <- ggplot(df, aes(x, y, label = z)) +
#'      geom_textpath(start_proportion = 0.25, size = 6)
#'
#' p
#'
#' p + coord_polar()
geom_textpath <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE,
                          start_proportion = 0.45, ...)
{
  ggplot2::layer(geom = GeomTextPath, mapping = mapping, data = data, stat = stat,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(start_proportion = start_proportion, ...))
}

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
