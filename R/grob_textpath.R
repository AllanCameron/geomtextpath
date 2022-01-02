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
#' @param gp_box (Optional) an object of class `"gpar"`, typically the output
#'   from a call to the [`gpar()`][grid::gpar] function. If this is an empty
#'   list, no text box will be drawn.
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#' @param as_label a `logical` TRUE or FALSE indicating whether the text should
#'   be drawn inside a text box. If FALSE, the parameters `label.padding`,
#'   `label.r` and `gp_box` will be ignored.
#' @param remove_long if TRUE, labels that are longer than their associated
#'   path will be removed.
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
  remove_long = FALSE,
  arrow = NULL,
  default.units = "npc",
  name = NULL,
  vp = NULL,
  as_label = FALSE
) {

  cl <- if (as_label) "labelpath" else "textpath"

  if (missing(label)) return(gTree(name = name, vp = vp, cl = cl))

  n_label <- length(label)
  id <- match(id, unique(id))
  id_lens <- run_len(id)

  check_grob_input(x, y, id, id_lens, n_label, angle)

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

  path <- data_frame(x = x, y = y, id = rep(seq_along(id_lens), id_lens),
                     line_x = x, line_y = y)


  if (text_smoothing != 0) path <- path_smoother(path, text_smoothing)

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
        gap           = gap,
        remove_long   = remove_long
      ),
      arrow = arrow
    ),
    name = name,
    vp = vp,
    cl = cl
  )
}

# makeContent -------------------------------------------------------------

#' @export
makeContent.textpath <- function(x) {

  if (is.null(x$textpath)) return(zeroGrob())
  v <- x$textpath
  x$textpath <- NULL
  params <- v$params

  path <- prepare_path(v$data, v$label, v$gp_path, params)

  too_long <- if(params$remove_long) {
    # Identify text that is too long for its path
    text_lens <- vapply(v$label, function(x) max(x$xmax), numeric(1))
    path_lens <- vapply(path,
                        function(d) max(arclength_from_xy(d$line_x, d$line_y)),
                        numeric(1))
    text_lens > path_lens
  } else {
    rep(FALSE, length(v$label))
  }

  ss <- v$data$id %in% which(too_long)

  if(any(too_long)) {

    x <- grid::addGrob(x, grid::polylineGrob(
        v$data$x[ss], v$data$y[ss], id = v$data$id[ss],
        gp = gp_subset(v$gp_path, too_long)
      ))
  }

  if(!all(too_long))
  {

  # Get the actual text string positions and angles for each group
  text <- Map(
      place_text,
      path = path[!too_long], label = v$label[!too_long],
      hjust = params$hjust[!too_long], halign = params$halign[!too_long],
      upright = params$upright
    )

  text <- rbind_dfs(text)

  x <- .add_path_grob(x, path[!too_long], text,
                      gp_subset(attr(path, "gp"), !too_long),
                      params, v$arrow)
  x <- .add_text_grob(x, text, gp_subset(v$gp_text, !too_long))
  }
  x
}


check_grob_input <- function(x, y, id, id_lens, n_label, angle) {

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
}
