#' @export
#' @rdname textpathGrob
#' @param gp_box (Optional) an object of class `"gpar"`, typically the output
#'   from a call to the [`gpar()`][grid::gpar] function. If this is an empty
#'   list, no text box will be drawn.
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
  keep_straight = FALSE,
  gp_text = gpar(),
  gp_path = gpar(),
  gp_box  = gpar(),
  cut_path = NA,
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

  if(missing(label)) return(gTree(name = name, vp = vp, cl = "labelpath"))

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
        cut_path      = cut_path
      ),
      arrow = arrow
    ),
    name = name,
    vp = vp,
    cl = "labelpath"
  )
}

#' @export
makeContent.labelpath <- function(x) {

  if(is.null(x$textpath)) return(zeroGrob())
  v <- x$textpath
  x$textpath <- NULL
  params <- v$params


  ## ---- Data manipulation -------------------------------------------- #
  path <- .prepare_path(v$data, v$label, v$gp_path, params)

  # Get the actual text string positions and angles for each group
  text <- Map(
    .get_path_points,
    path = path, label = v$label,
    hjust = params$hjust, halign = params$halign,
    flip_inverted = params$flip_inverted
  )

  if ({make_box <- sum(lengths(v$gp_box))}) {
    box <- Map(
      .curved_textbox,
      path = path, label = v$label, text = text,
      padding = params$label.padding, radius = params$label.r
    )
    box <- rbind_dfs(box)
  }

  text <- rbind_dfs(text)

  x <- .add_path_grob(x, path, text, attr(path, "gp"), params, v$arrow)

  if (make_box) {
    x <- addGrob(
      x, polygonGrob(
        x = box$x, y = box$y, id = box$id,
        default.units = "inches", gp = v$gp_box
      )
    )
  }

  x <- .add_text_grob(x, text, v$gp_text)
  x
}
