# Constructor -------------------------------------------------------------

#' @export
#' @rdname textpathGrob
#' @param gp_box (Optional) an object of class `"gpar"`, typically the output
#'   from a call to the [`gpar()`][grid::gpar] function. If this is an empty
#'   list, no text box will be drawn.
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
labelpathGrob <- function(
  label,
  x = 0.5,
  y = 0.5,
  id = 1L,
  just = "centre",
  hjust = NULL,
  vjust = NULL,
  halign = "left",
  angle = 0,
  straight = FALSE,
  gp_text = gpar(),
  gp_path = gpar(),
  gp_box  = gpar(),
  gap = NA,
  upright = TRUE,
  rich = FALSE,
  polar_params = NULL,
  padding = unit(0.15, "inch"),
  label.padding = unit(0.25, "lines"),
  label.r = unit(0.15, "lines"),
  arrow = NULL,
  default.units = "npc",
  name = NULL,
  vp = NULL
) {

  if (missing(label)) return(gTree(name = name, vp = vp, cl = "labelpath"))

  n_label <- length(label)
  id_lens <- run_len(id)

  # Verify that:
  #  1) There are as many labels as there are paths
  #  2) There are as many x's as y's (or one is of length 1)
  #  3) There are as many x's as id's (or one is of length 1)

  stopifnot(
    "`x` is not of the same length as `id`" =
      length(x) == length(id),
    "`y` is not the same length as `x`" =
      length(x) == length(y),
    "Cannot match labels to paths." =
      n_label == length(id_lens),
    "`angle` must be length 1 or the same length as `x`." =
      (length(x) == length(angle)) || length(angle) == 1
  )

  # Match justification to labels length
  hjust  <- rep_len(resolveHJust(just, hjust), n_label)
  vjust  <- rep_len(resolveVJust(just, vjust), n_label)
  halign <- rep_len(halign, n_label)

  label <- measure_label(label, gp = gp_text, vjust = vjust,
                         halign = halign, straight = straight,
                         rich = rich)

  x <- as_unit(x, default.units)
  y <- as_unit(y, default.units)

  if (!is.null(polar_params)) {
    polar_params$x <- unit(polar_params$x, default.units)
    polar_params$y <- unit(polar_params$y, default.units)
  }

  path <- data_frame(x = x, y = y, id = rep(seq_along(id_lens), id_lens))

  gTree(
    textpath = list(
      data          = path,
      label         = label,
      gp_text       = attr(label, "gp"),
      gp_path       = gp_path,
      gp_box        = gp_box,
      params = list(
        upright       = upright,
        polar_params  = polar_params,
        angle         = angle,
        padding       = padding,
        label.padding = label.padding,
        label.r       = label.r,
        hjust         = hjust,
        vjust         = vjust,
        halign        = halign,
        gap           = gap
      ),
      arrow = arrow
    ),
    name = name,
    vp = vp,
    cl = "labelpath"
  )
}

# Drawing -----------------------------------------------------------------

#' @export
makeContent.labelpath <- function(x) {

  if (is.null(x$textpath)) return(zeroGrob())
  v <- x$textpath
  x$textpath <- NULL
  params <- v$params


  ## ---- Data manipulation -------------------------------------------- #
  path <- prepare_path(v$data, v$label, v$gp_path, params)

  # Get the actual text string positions and angles for each group
  text <- Map(
    place_text,
    path = path, label = v$label,
    hjust = params$hjust, halign = params$halign,
    upright = params$upright
  )
  ntext <- length(text)

  # Get points on the box
  if ({make_box <- sum(lengths(v$gp_box))}) {
    box <- Map(
      curved_textbox,
      path = path, label = v$label, text = text,
      padding = params$label.padding, radius = params$label.r
    )
    box <- rbind_dfs(box)
  }

  text <- rbind_dfs(text)

  x <- .add_path_grob(x, path, text, attr(path, "gp"), params, v$arrow)

  # Construct textbox grobs as list
  if (make_box) {
    boxgrob <- lapply(seq_len(ntext), function(i) {
      gp  <- recycle_gp(v$gp_box, function(x) x[pmin(i, length(x))])
      dat <- box[box$id == i, , drop = FALSE]
      polygonGrob(
        x = dat$x, y = dat$y,
        default.units = "inches", gp = gp
      )
    })
  } else {
    boxgrob <- NULL
  }

  # Construct text grobs as list
  textgrob <- lapply(seq_len(ntext), function(i) {
    dat <- text[text$id == i, , drop = FALSE]
    sub <- unlist(dat$substring, FALSE, FALSE) %||% dat$id
    gp  <- recycle_gp(v$gp_text, function(x) x[pmin(sub, length(x))])

    if (is.list(dat$xoffset)) {
      xx <- unlist(dat$xoffset, FALSE, FALSE)
      yy <- unlist(dat$yoffset, FALSE, FALSE)
      angle <- dat$angle * .deg2rad
      x <- xx * cos(angle) - yy * sin(angle) + dat$x
      y <- xx * sin(angle) + yy * cos(angle) + dat$y
    } else {
      x <- dat$x
      y <- dat$y
    }

    textGrob(
      label = make_label(dat$label),
      x = x,
      y = y,
      rot = dat$angle,
      vjust = 0.5, hjust = 0.5, gp = gp,
      default.units = "inches"
    )
  })

  # Alternate box and textgrobs
  grobs <- rbind(boxgrob, textgrob)
  # Add box and textgrobs
  grobs <- do.call(gList, c(x$children[1], grobs))
  x <- setChildren(x, grobs)
  x
}

#' Making a curved textbox
#'
#' @param path A `data.frame` containing `x` and `y` columns with numeric values.
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
  height <- c(0, 1) * metrics$height + c(-1, 1) * padding

  if (nrow(text) > 1) {
    # Get min / mid / max height
    height <- c(0, 1) * metrics$height + c(-1, 1) * padding
    height <- c(height[1], sum(height) / 2, height[2])

    # Apply height to minimal offset
    offset <- as_inch(attr(label, "offset"))[unique(label$y_id)]
    offset <- min(offset) + metrics$x_adj
    offset <- c(0, offset + height)

    # Calculate offsets
    offset <- get_offset(path$x, path$y, offset)

    # Set start / end at 0-offset, then translate to mid-height offset
    lims <- range(text$left, text$right)
    lims <- approx_multiple(offset$arc_length[, 1], lims, offset$arc_length[, 3])
    lims <- lims + c(-1, 1) * padding

    # Translate mid-height offset to min / max height offsets to get corners
    corners <- approx_multiple(
      offset$arc_length[, 3], lims,
      y = cbind(offset$x[, c(2, 4)], offset$y[, c(2, 4)])
    )

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
    xx <- (metrics$width  * c(-0.5, 0.5) + c(-padding, padding))[c(1, 2, 2, 1)]
    yy <- (diff(height)   * c(-0.5, 0.5))[c(1, 1, 2, 2)]
    rot <- text$angle * .deg2rad
    x <- xx * cos(rot) - yy * sin(rot) + text$x
    y <- xx * sin(rot) + yy * cos(rot) + text$y
    nkeep <- 0
  }

  radius <- as_inch(radius)
  if (radius > 0.01) {
    if (radius > 0.5 * diff(range(height))) {
      radius  <- 0.5 * diff(range(height))
    }
    # Make closed polygon by inserting midpoint between start and end
    n <- length(x)
    x_start <- (x[1] + x[n]) / 2
    y_start <- (y[1] + y[n]) / 2
    x <- c(x_start, x, x_start)
    y <- c(y_start, y, y_start)

    # Round corners
    corners <- c(2, 3 + nkeep, 4 + nkeep, 5 + 2 * nkeep)
    xy <- round_corners(x, y, radius, at = corners)
    x <- xy$x
    y <- xy$y
  }
  return(data_frame(x = x, y = y, id = text$id[1]))
}

