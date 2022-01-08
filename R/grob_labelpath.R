##---------------------------------------------------------------------------##
##                                                                           ##
##  grob_labelpath.R                                                         ##
##  Part of the geomtextpath R package                                       ##
##                                                                           ##
##  Copyright (C) 2021 by Allan Cameron & Teun van den Brand                 ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

#' @export
makeContent.labelpath <- function(x) {
  if (is.null(x$textpath)) return(zeroGrob())

  v          <- x$textpath
  x$textpath <- NULL
  params     <- v$params
  path       <- prepare_path(v$data, v$label, v$gp_path, params)

  # Identify text that is too long for its path
  if (params$remove_long) {
    text_lens <- numapply(v$label, function(x) max(x$xmax))
    path_lens <- numapply(path, function(d) {
                   max(arclength_from_xy(d$line_x, d$line_y))})
    too_long  <- text_lens > path_lens
  } else {
    too_long  <- rep(FALSE, length(v$label))
  }

  ss <- v$data$id %in% which(too_long)

  if (any(too_long)) {
    x <- addGrob(x, polylineGrob(x  = v$data$x[ss],
                                 y  = v$data$y[ss],
                                 id = v$data$id[ss],
                                 gp = gp_subset(v$gp_path, too_long))
                 )
  }

  if (!all(too_long)) {
    # Get the actual text string positions and angles for each group
    text <- Map(
        f       = place_text,
        path    = path[!too_long], label = v$label[!too_long],
        hjust   = params$hjust[!too_long], halign = params$halign[!too_long],
        upright = params$upright
      )

    ntext <- which(!too_long)

    # Get points on the box
    if (sum(lengths(v$gp_box))) {
      box <- Map(
        f       = curved_textbox,
        path    = path[!too_long],
        label   = v$label[!too_long],
        text    = text,
        padding = params$label.padding,
        radius  = params$label.r
      )
      box <- rbind_dfs(box)
    }

    text <- rbind_dfs(text)

    x <- add_path_grob(x, path[!too_long], text,
                        gp_subset(attr(path, "gp"), !too_long), params, v$arrow)

    # Construct textbox grobs as list
    if (sum(lengths(subset(v$gp_box, !too_long)))) {
      boxgrob <- lapply(ntext, function(i) {
        gp  <- recycle_gp(gp_subset(v$gp_box, !too_long),
                          function(x) x[pmin(i, length(x))])
        dat <- box[box$id == i, , drop = FALSE]
        polygonGrob(x = dat$x, y = dat$y, default.units = "inches", gp = gp)
      })
    } else {
      boxgrob <- NULL
    }

    # Construct text grobs as list
    textgrob <- lapply(ntext, function(i) {
      dat <- text[text$id == i, , drop = FALSE]
      sub <- unlist(dat$substring, FALSE, FALSE) %||% dat$id
      gp  <- recycle_gp(subset(v$gp_text, !too_long),
                        function(x) x[pmin(sub, length(x))])
      if (is.list(dat$xoffset)) {
        xx    <- unlist(dat$xoffset, FALSE, FALSE)
        yy    <- unlist(dat$yoffset, FALSE, FALSE)
        angle <- dat$angle * .deg2rad
        x     <- xx * cos(angle) - yy * sin(angle) + dat$x
        y     <- xx * sin(angle) + yy * cos(angle) + dat$y
      } else {
        x     <- dat$x
        y     <- dat$y
      }
      textGrob(
        label         = make_label(dat$label),
        x             = x,
        y             = y,
        rot           = dat$angle,
        vjust         = 0.5, hjust = 0.5, gp = gp,
        default.units = "inches"
      )
    })

    # Alternate box and textgrobs
    grobs <- rbind(boxgrob, textgrob)
    # Add box and textgrobs
    grobs <- do.call(gList, c(x$children[1], grobs))
    x <- setChildren(x, grobs)
  }
  x
}

#' Making a curved textbox
#'
#' @param path A `data.frame` containing `x` and `y` columns with numeric values
#' @param text A `data.frame` containing `left` and `right` vectors with values
#'   along the arc-length of a path where text appears, and an `id` column.
#' @param label A `data.frame` as produced by `measure_text()`.
#' @param padding A `grid::unit()`.
#' @param radius A `grid::unit()`.
#'
#' @return A `data.frame` containing `x`, `y` and `id` columns for a closed
#'   polygon.
#' @noRd
#'
#' @examples
#' NULL
curved_textbox <- function(
  path,
  text,
  label,
  padding = unit(0.25, "lines"),
  radius  = unit(0.15, "lines")
) {
  padding <- as_inch(padding)
  metrics <- attr(label, "metrics")
  height  <- c(0, 1) * metrics$height + c(-1, 1) * padding

  if (nrow(text) > 1) {
    # Get min / mid / max height
    height <- c(0, 1) * metrics$height + c(-1, 1) * padding
    height <- c(height[1], sum(height) / 2, height[2])

    # Apply height to minimal offset
    offset <- as_inch(attr(label, "offset"))[unique(label$y_id)]
    offset <- min(offset) - 0.5 * metrics$lineheight
    offset <- c(0, offset + height)

    # Calculate offsets
    offset <- get_offset(path$x, path$y, offset)

    # Set start / end at 0-offset, then translate to mid-height offset
    lims <- range(text$left, text$right)
    lims <- approx_multi(offset$arc_length[, 1], offset$arc_length[, 3], lims)
    lims <- lims + c(-1, 1) * padding

    # Translate mid-height offset to min / max height offsets to get corners
    corners <- approx_multi(x = offset$arc_length[, 3],
                            y = cbind(offset$x[, c(2, 4)], offset$y[, c(2, 4)]),
                            xout = lims)

    # Check which points fall between corners
    keep  <- offset$arc_length[, 3] > lims[1] & offset$arc_length[, 3] < lims[2]
    nkeep <- sum(keep)

    # Add points in-between corners
    x <- c(corners[1, 1], offset$x[keep, 2],      corners[2, 1],
           corners[2, 2], rev(offset$x[keep, 4]), corners[1, 2])
    y <- c(corners[1, 3], offset$y[keep, 2],      corners[2, 3],
           corners[2, 4], rev(offset$y[keep, 4]), corners[1, 4])
  } else {
    # Simple rotation of a rectangle
    xx  <- (metrics$width  * c(-0.5, 0.5) + c(-padding, padding))[c(1, 2, 2, 1)]
    yy  <- (diff(height)   * c(-0.5, 0.5))[c(1, 1, 2, 2)]
    rot <- text$angle * .deg2rad
    x   <- xx * cos(rot) - yy * sin(rot) + text$x
    y   <- xx * sin(rot) + yy * cos(rot) + text$y
    nkeep <- 0
  }

  radius <- as_inch(radius)
  if (radius > 0.01) {
    if (radius > 0.5 * diff(range(height))) {
      radius  <- 0.5 * diff(range(height))
    }
    # Make closed polygon by inserting midpoint between start and end
    n       <- length(x)
    x_start <- (x[1] + x[n]) / 2
    y_start <- (y[1] + y[n]) / 2
    x       <- c(x_start, x, x_start)
    y       <- c(y_start, y, y_start)

    # Round corners
    corners <- c(2, 3 + nkeep, 4 + nkeep, 5 + 2 * nkeep)
    xy      <- round_corners(x, y, radius, at = corners)
    x       <- xy$x
    y       <- xy$y
  }
  return(data_frame(x = x, y = y, id = text$id[1]))
}
