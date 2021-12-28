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
#' @inheritParams grid::polylineGrob
#' @inheritParams static_text_params
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
  straight = FALSE,
  rich     = FALSE,
  gp_text = gpar(),
  gp_path = gpar(),
  gp_box  = gpar(),
  gap = NA,
  upright = TRUE,
  text_smoothing = 0,
  polar_params = NULL,
  padding = unit(0.15, "inch"),
  label.padding = unit(0.25, "lines"),
  label.r = unit(0.15, "lines"),
  arrow = NULL,
  default.units = "npc",
  name = NULL,
  vp = NULL
) {

  if (missing(label)) return(gTree(name = name, vp = vp, cl = "textpath"))

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

  path$map <- arclength_from_xy(x, y, path$id)

  text_path <- if (text_smoothing != 0) {
    path_smoother(path, text_smoothing)
  } else {
    path
  }

  gTree(
    textpath = list(
      data          = path,
      text_path     = text_path,
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
    cl = "textpath"
  )
}

# makeContent -------------------------------------------------------------

#' @export
makeContent.textpath <- function(x) {

  if (is.null(x$textpath)) return(zeroGrob())
  v <- x$textpath
  x$textpath <- NULL
  params <- v$params

  line <- prepare_path(v$data, v$label, v$gp_path, params)
  text_path <- prepare_path(v$text_path, v$label, v$gp_path, params)

  # Get the actual text string positions and angles for each group
  text <- Map(
      place_text,
      path = text_path, label = v$label,
      hjust = params$hjust, halign = params$halign,
      upright = params$upright
    )

  text <- rbind_dfs(text)

  x <- .add_path_grob(x, line, text, attr(line, "gp"), params, v$arrow)
  x <- .add_text_grob(x, text, v$gp_text)
  x
}
