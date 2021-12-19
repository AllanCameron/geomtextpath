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
#'   describing the angle in degrees at which text should be rotated.
#' @param polar_params a list consisting of an x, y, and r component that
#'   specifies the central point and radius of a circle around which
#'   single-point labels will be wrapped.
#' @param arrow Arrow specification, as created by [`arrow()`][grid::arrow].
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
  gp_box  = gpar(),
  gap = NA,
  flip_inverted = TRUE,
  polar_params = NULL,
  padding = unit(0.15, "inch"),
  label.padding = unit(0.25, "lines"),
  label.r = unit(0.15, "lines"),
  arrow = NULL,
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

  label <- measure_text(label, gp_text, vjust = vjust, halign = halign,
                        straight = keep_straight)

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
      gp_text       = gp_text,
      gp_path       = gp_path,
      gp_box        = gp_box,
      params = list(
        flip_inverted = flip_inverted,
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
    cl = "textpath"
  )
}

# makeContent -------------------------------------------------------------

#' @export
makeContent.textpath <- function(x) {

  if(is.null(x$textpath)) return(zeroGrob())
  v <- x$textpath
  x$textpath <- NULL
  params <- v$params

  path <- .prepare_path(v$data, v$label, v$gp_path, params)

  # Get the actual text string positions and angles for each group
  text <- Map(
      .get_path_points,
      path = path, label = v$label,
      hjust = params$hjust, halign = params$halign,
      flip_inverted = params$flip_inverted
    )
  text <- rbind_dfs(text)

  x <- .add_path_grob(x, path, text, attr(path, "gp"), params, v$arrow)
  x <- .add_text_grob(x, text, v$gp_text)
  x
}

.prepare_path <- function(data, label, gp, params) {
  path <- dedup_path(
    x = as_inch(data$x, "x"),
    y = as_inch(data$y, "y"),
    id = data$id
  )
  path <- split(path, path$id)

  if (any({singletons <- vapply(path, nrow, integer(1)) == 1})) {
    width <- vapply(label, function(x) max(x$xmax, na.rm = TRUE), numeric(1))
    path[singletons] <- Map(.pathify,
                            data    = path[singletons],
                            hjust   = params$hjust[singletons],
                            angle   = params$angle,
                            width   = width[singletons],
                            polar_x = list(params$polar_params$x),
                            polar_y = list(params$polar_params$y),
                            thet    = list(params$polar_params$theta))
    gp$lty[singletons] <- 0
  }
  attr(path, "gp") <- gp
  return(path)
}

.add_path_grob <- function(grob, data, text, gp, params, arrow = NULL) {
  has_line <- !all((gp$lty %||% 1)  %in% c("0", "blank", NA))
  is_opaque <-!all((gp$col %||% 1) %in% c(NA, "transparent"))
  if (has_line && is_opaque) {
    data <- rbind_dfs(data)

    # Get bookends by trimming paths when it intersects text
    data <- .get_surrounding_lines(
      data, text,
      vjust   = params$vjust    %||% 0.5,
      gap     = params$gap      %||% NA,
      padding = params$padding  %||% 0.15
    )
    arrow <- .tailor_arrow(data, arrow)

    if (nrow(data) > 1) {
      # Recycle graphical parameters to match lengths of path
      gp <- recycle_gp(gp, `[`, i = data$id[data$start])
      gp$fill <- gp$col

      # Write path grob
      grob <- addGrob(
        grob, polylineGrob(
          x = data$x, y = data$y, id = data$new_id, gp = gp,
          default.units = "inches",
          arrow = arrow
        )
      )
    }
  }
  return(grob)
}

.add_text_grob <- function(grob, text, gp) {
  text_lens <- run_len(text$id)

  # Recycle graphical parameters to match lengths of letters
  gp <- recycle_gp(gp, rep, times = text_lens)

  # Write text grob
  grob <- addGrob(
    grob, textGrob(
      label = make_label(text$label),
      x = text$x, y = text$y, rot = text$angle,
      vjust = 0.5, hjust = 0.5, gp = gp,
      default.units = "inches"
    )
  )
}


# Graphical parameters helper ---------------------------------------------

# Helper function to do safe(r) recycling on "gpar" class objects.
recycle_gp <- function(gp, fun, ...) {
  # Recycling rules only apply to non-unique parameters
  do_recycle <- lengths(gp) > 1
  gp[do_recycle] <- lapply(unclass(gp)[do_recycle], fun, ...)
  # Never ever have zero-length objects in the gpar
  gp[lengths(gp) == 0] <- list(NULL)
  return(gp)
}

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

.pathify <- function(data, hjust, angle, width,
                     polar_x = NULL, polar_y = NULL, thet = NULL) {

  angle <- pi * angle / 180
  multi_seq <- Vectorize(seq.default)

   if(!is.null(polar_x) & !is.null(polar_y) & !is.null(thet)) {

     polar_x <- as_inch(polar_x, "x")
     polar_y <- as_inch(polar_y, "y")

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

# This adjusts a possible arrow to not have duplicated arrowheads when a path
# is cut into two due to the path trimming.
.tailor_arrow <- function(data, arrow) {
  if (is.null(arrow)) {
    return(arrow)
  }
  keep  <- !duplicated(data$new_id)
  sides <- data$section[keep]
  id    <- data$id[keep]
  path  <- data

  # Have arrow match the length of the groups
  arrow[] <- lapply(arrow, function(x) {
    x[pmin(id, length(x))]
  })
  angle <- arrow$angle
  ends  <- arrow$ends

  # Ends are 1 = "first", 2 = "last", 3 = "both".
  # We 'hide' an arrow by setting an NA angle
  angle[ends == 2 & sides == "pre"]  <- NA_integer_
  angle[ends == 1 & sides == "post"] <- NA_integer_
  ends[ends == 3 & sides == "pre"]   <- 1L
  ends[ends == 3 & sides == "post"]  <- 2L
  arrow$angle <- angle
  arrow$ends  <- ends
  arrow
}



