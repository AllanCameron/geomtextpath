# Grob --------------------------------------------------------------------

#' Draw text on a path.
#'
#' This function creates (curved) text on a path.
#'
#' @param label A `character` vector.
#' @param x A `numeric` vector.
#' @param y A `numeric` vector.
#' @param id A `numeric` vector used to separate locations in `x` and `y` into
#'   multiple lines. All locations with the same `id` belong to the same line.
#' @param gp_text,gp_path An object of class `"gpar"`, typically the output from
#'   a call from the [`gpar()`][grid::gpar] function. These are basically lists
#'   of graphical parameters for the text and path respectively.
#' @param vjust A `numeric` vector specifying justification orthogonal to the
#'   direction of the text. Alternatively a [`unit()`][grid::unit()] object to
#'   directly set the offset from the path.
#' @param angle a `numeric` vector either length 1 or the same length as `id`
#'   describing the angle at which text should be rotated.
#' @param keep_straight a logical **TRUE** or **FALSE** indicating whether the
#'   text should be straight rather than following the curve. This might be
#'   helpful for noisy paths. If **TRUE** the text will still follow the angle
#'   of the curve. The default is **FALSE**
#' @param polar_params a list consisting of an x, y, and r component that
#'   specifies the central point and radius of a circle around which
#'   single-point labels will be wrapped.
#' @inheritParams grid::textGrob
#' @inheritParams geom_textpath
#'
#' @return An object of class `gTree`, containing grobs.
#' @export
#' @md
#'
#' @examples
#'require(grid)
#'
#' t <- seq(0, 2 * pi, length.out = 100)
#' grob <- textpathGrob(
#'   label = c(
#'     "Why I am making trigonometry jokes? Cos I can!",
#'     "I was never any good at sine language."
#'   ),
#'   x = c(t, t) / (2 * pi),
#'   y = c(cos(t), sin(t)) * 0.25 + 0.5,
#'   id = rep(1:2, each = length(t)),
#'   vjust = rep(0.5, 2 * length(t)),
#'   gp_text = gpar(lineheight = c(1.2, 1.2), fontsize = c(10, 10)),
#'   gp_path = gpar(lty = c(1, 2))
#' )
#'
#' grid.newpage(); grid.draw(grob)
textpathGrob <- function(
  label,
  x = 0.5,
  y = 0.5,
  id = 1L,
  just = "centre",
  hjust = NULL,
  vjust = NULL,
  halign = "left",
  angle = 0,
  keep_straight = FALSE,
  gp_text = gpar(),
  gp_path = gpar(),
  cut_path = NA,
  flip_inverted = TRUE,
  polar_params = NULL,
  padding = unit(0.15, "inch"),
  default.units = "npc",
  name = NULL,
  vp = NULL
) {

  if(missing(label)) return(gTree(name = name, vp = vp, cl = "textpath"))

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

  # Reconstitute data
  gp_text <- gp_fill_defaults(gp_text)

  if(length(gp_text$fontsize) == 1) {
    gp_text$fontsize <- rep(gp_text$fontsize, n_label)
  }

  if(is.language(label) || keep_straight)
  {
    label <- measure_exp(label, gp_text, vjust = vjust)

  } else {
    label <- as.character(label)
    label <- measure_text(label, gp_text, vjust = vjust, halign = halign)
  }

  if (!is.unit(x)) {
    x <- unit(x, default.units)
  }
  if (!is.unit(y)) {
    y <- unit(y, default.units)
  }

  if (!is.null(polar_params))
  {
    polar_params$x <- unit(polar_params$x, default.units)
    polar_params$y <- unit(polar_params$y, default.units)
  } else {
    polar_params <- list(x = NA, y = NA, theta = NA)
  }

  path <- data_frame(x = x, y = y, id = rep(seq_along(id_lens), id_lens))

  gTree(
    textpath = list(
      data          = path,
      label         = label,
      hjust         = hjust,
      vjust         = vjust,
      halign        = halign,
      cut_path      = cut_path,
      gp_text       = gp_text,
      gp_path       = gp_path,
      flip_inverted = flip_inverted,
      polar_params  = polar_params %||% list(x = NA, y = NA, theta = NA),
      angle         = angle,
      padding       = padding
    ),
    name = name,
    vp = vp,
    cl = "textpath"
  )
}

# makeContent -------------------------------------------------------------

#' @export
makeContent.textpath <- function(x) {

  if(is.null(x$textpath)) return(zeroGrob())
  v <- x$textpath
  path <- dedup_path(
    x = convertX(v$data$x, "inches", valueOnly = TRUE),
    y = convertY(v$data$y, "inches", valueOnly = TRUE),
    id = v$data$id
  )
  if(is.unit(v$polar_params$x) & is.unit(v$polar_params$y))
  {
    v$polar_params$x <- convertX(v$polar_params$x, "inches", valueOnly = TRUE)
    v$polar_params$y <- convertY(v$polar_params$y, "inches", valueOnly = TRUE)
  }
  x$textpath <- NULL

  ## ---- Data manipulation -------------------------------------------- #

  path$size <- rep(v$gp_text$fontsize, run_len(path$id))

  # Get gradients, angles and path lengths for each group
  path <- split(path, path$id)

  # Handle point-like textpaths
  if (any({singletons <- vapply(path, nrow, integer(1)) == 1})) {
    width <- vapply(v$label, function(x) max(x$xmax, na.rm = TRUE), numeric(1))
    path[singletons] <- Map(.pathify,
                            data    = path[singletons],
                            hjust   = v$hjust[singletons],
                            angle   = v$angle,
                            width   = width[singletons],
                            polar_x = v$polar_params$x,
                            polar_y = v$polar_params$y,
                            thet    = v$polar_params$theta)
    v$gp_path$lty[singletons] <- 0
  }


  path <- lapply(path, function(p) {
    p$length <-.arclength_from_xy(p$x, p$y)
    p
  })


  # Get the actual text string positions and angles for each group
  text <- Map(
      .get_path_points,
      path = path, label = v$label,
      hjust = v$hjust, halign = v$halign,
      flip_inverted = v$flip_inverted
    )
  text_lens <- vapply(text, nrow, integer(1))
  text <- rbind_dfs(text)

  if (!all((v$gp_path$lty %||% 1) %in% c("0", "blank", NA))) {
    path <- rbind_dfs(path)

    # Get bookends by trimming paths when it intersects text
    path <- .get_surrounding_lines(path, text, vjust = v$vjust, v$cut_path,
                                   breathing_room = v$padding)

    if (nrow(path) > 1) {
      # Recycle graphical parameters to match lengths of path
      gp_path <- recycle_gp(v$gp_path, `[`, i = path$id[path$start])

      # Write path grob
      x <- addGrob(
        x, polylineGrob(
          x = path$x, y = path$y, id = path$new_id, gp = gp_path,
          default.units = "inches"
        )
      )
    }
  }

  # Recycle graphical parameters to match lengths of letters
  gp_text <- recycle_gp(v$gp_text, rep, times = text_lens)

  # Write text grob
  x <- addGrob(
    x, textGrob(
      label = make_label(text$label),
      x = text$x, y = text$y, rot = text$angle,
      vjust = 0.5, hjust = 0.5, gp = gp_text,
      default.units = "inches"
    )
  )
  x
}


# recycle_gp ---------------------------------------------------------------

# Helper function to do safe(r) recycling on "gpar" class objects.
recycle_gp <- function(gp, fun, ...) {
  # Recycling rules only apply to non-unique parameters
  do_recycle <- lengths(gp) > 1
  gp[do_recycle] <- lapply(unclass(gp)[do_recycle], fun, ...)
  # Never ever have zero-length objects in the gpar
  gp[lengths(gp) == 0] <- list(NULL)
  return(gp)
}

# gp_fill_defaults -------------------------------------------------------

# Helper function to fill in missing parameters by defaults
# Based on ggplot2:::modify_list
gp_fill_defaults <- function(gp, ..., defaults = get.gpar()) {
  extra <- list(...)
  for (i in names(extra)) defaults[[i]] <- extra[[i]]
  for (i in names(gp))    defaults[[i]] <- gp[[i]]
  defaults
}

# dedup_path -------------------------------------------------------------

# Path constructor that filters out subsequent duplicated points that can cause
# problems for gradient/offset calculations. Also interpolates any NA values in
# the x, y values to avoid broken paths, and removes any points that have an
# NA id.

dedup_path <- function(x, y, id, tolerance = 1000 * .Machine$double.eps) {

  vecs <- data_frame(x = .interp_na(x), y = .interp_na(y), id = id)
  lens <- lengths(vecs)
  n    <- max(lengths(vecs))
  vecs[lens != n] <- lapply(vecs[lens != n], rep_len, length.out = n)

  dups <- vapply(vecs, function(x){abs(x[-1] - x[-length(x)]) < tolerance},
                 logical(n - 1))
  if (n > 2) {
    keep <- c(TRUE, rowSums(dups) < 3L)
  } else {
    keep <- c(TRUE, sum(dups) < 3L)
  }
  vecs <- vecs[keep, , drop = FALSE]

  vecs[complete.cases(vecs),]
}


# Convert point-like textpaths into proper text paths.

.pathify <- function(data, hjust, angle, width, polar_x, polar_y, thet) {

  angle <- pi * angle / 180
  multi_seq <- Vectorize(seq.default)

   if(!is.na(polar_x) & !is.na(polar_y)) {

     if(thet == "y") angle <- angle - pi/2
     r <- sqrt((data$x - polar_x)^2 + (data$y - polar_y)^2)
     width <- width / r
     theta <- atan2(data$y - polar_y, data$x - polar_x)
     theta_min  <- theta + cos(angle + pi) * width * hjust
     theta_max  <- theta + cos(angle) * width * (1 - hjust)
     r_min   <- r + sin(angle + pi) * width * hjust
     r_max   <- r + sin(angle) * width * (1 - hjust)

     theta <- c(multi_seq(theta_min, theta_max, length.out = 100))
     r <- c(multi_seq(r_min, r_max, length.out = 100))
     x <- polar_x + r * cos(theta)
     y <- polar_y + r * sin(theta)
   }
   else
   {
     xmin  <- data$x + cos(angle + pi) * width * hjust
     xmax  <- data$x + cos(angle) * width * (1 - hjust)
     ymin  <- data$y + sin(angle + pi) * width * hjust
     ymax  <- data$y + sin(angle) * width * (1 - hjust)
     x <- c(multi_seq(xmin, xmax, length.out = 100))
     y <- c(multi_seq(ymin, ymax, length.out = 100))
   }

   data <- data[rep(seq(nrow(data)), each = 100),]
   data$x <- x
   data$y <- y
   data
}



