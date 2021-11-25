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
#' @param cut_path A single logical TRUE or FALSE which if TRUE breaks the path
#'   into two sections, one on either side of the string and if FALSE leaves the
#'   path unbroken. The default value is NA, which will break the line if the
#'   string has a vjust of between 0 and 1
#' @param flip_inverted If TRUE, any string where the majority of letters would
#'   be upside down along the path are inverted to improve legibility. The
#'   default is FALSE.
#' @param halign A `character(1)` setting text alignment amongst multi-line
#'   labels. Can be either `"left"` (default), `"center"` or `"right"`.
#'
#' @inheritParams grid::textGrob
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
  gp_text = gpar(),
  gp_path = gpar(),
  cut_path = NA,
  flip_inverted = FALSE,
  default.units = "npc",
  name = NULL,
  vp = NULL
) {

  n_label <- length(label)
  id_lens <- run_len(id)

  # Verify that:
  #  1) There are as many labels as there are paths
  #  2) There are as many x's as y's (or one is of length 1)
  #  3) There are as many x's as id's (or one is of length 1)

  stopifnot(
    "`x` is not of the same length as `id`" =
      length(x) == length(id) || length(id) == 1 || length(x) == 1,
    "`y` is not the same length as `x`" =
      length(x) == length(y)  || length(y) == 1  || length(x) == 1,
    "Cannot match labels to paths." =
      n_label == length(id_lens)
  )

  # Match justification to labels length
  hjust  <- rep_len(resolveHJust(just, hjust), n_label)
  vjust  <- rep_len(resolveVJust(just, vjust), n_label)
  halign <- rep_len(halign, n_label)

  # Reconstitute data
  gp_text <- gp_fill_defaults(gp_text)

  if (!is.unit(x)) {
    x <- unit(x, default.units)
  }
  if (!is.unit(y)) {
    y <- unit(y, default.units)
  }

  path <- dedup_path(x = x, y = y, id = rep(seq_along(id_lens), id_lens))

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
      flip_inverted = flip_inverted
    ),
    name = name,
    vp = vp,
    cl = "textpath"
  )
}

# makeContent -------------------------------------------------------------

#' @export
makeContent.textpath <- function(x) {
  v <- x$textpath
  path <- v$data
  x$textpath <- NULL

  path$x <- convertX(path$x, "inches", valueOnly = TRUE)
  path$y <- convertY(path$y, "inches", valueOnly = TRUE)

  ## ---- Data manipulation -------------------------------------------- #

  path$size <- rep(v$gp_text$fontsize, run_len(path$id))

  # Get gradients, angles and path lengths for each group
  path <- split(path, path$id)

  path <- lapply(path, function(x){
    x$length <- .arclength_from_xy(x$x, x$y)
    x
  })

  # Get the actual text string positions and angles for each group
  text <- Map(
    .get_path_points,
    path = path, label = v$label,
    hjust = v$hjust, vjust = v$vjust, halign = v$halign,
    gp = split_gp(v$gp_text, seq_along(v$label)),
    flip_inverted = v$flip_inverted
  )
  text_lens <- vapply(text, nrow, integer(1))

  ## ---- Build text grob ---------------------------------------------- #

  text <- rbind_dfs(text)

  if (!all(v$gp_path$lty == 0)) {
    path <- rbind_dfs(path)

    # Get bookends by trimming paths when it intersects text
    path <- .get_surrounding_lines(path, text, vjust = v$vjust, v$cut_path)

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
      label = text$label,
      x = text$x, y = text$y, rot = text$angle,
      vjust = 0, hjust = 0.5, gp = gp_text,
      default.units = "inches"
    )
  )
  x
}

# Helpers -----------------------------------------------------------------

# Helper function to do safe(r) recycling on "gpar" class objects.
recycle_gp <- function(gp, fun, ...) {
  # Recycling rules only apply to non-unique parameters
  do_recycle <- lengths(gp) > 1
  gp[do_recycle] <- lapply(unclass(gp)[do_recycle], fun, ...)
  # Never ever have zero-length objects in the gpar
  gp[lengths(gp) == 0] <- list(NULL)
  return(gp)
}

# Helper function to split out "gpar" class objects by index
split_gp <- function(gp, index) {
  do_split <- lengths(gp) > 1
  lapply(index, function(i) {
    ans <- gp
    ans[do_split] <- lapply(unclass(gp)[do_split], `[`, i)
    ans[lengths(ans) == 0] <- list(NULL)
    ans
  })
}

# Helper function to fill in missing parameters by defaults
# Based on ggplot2:::modify_list
gp_fill_defaults <- function(gp, ..., defaults = get.gpar()) {
  extra <- list(...)
  for (i in names(extra)) defaults[[i]] <- extra[[i]]
  for (i in names(gp))    defaults[[i]] <- gp[[i]]
  defaults
}

# Path constructor that filters out subsequent duplicated points that can cause
# problems for gradient/offset calculations
dedup_path <- function(x, y, id, tolerance = 1000 * .Machine$double.eps) {
  vecs <- list(x = x, y = y, id = id)
  lens <- lengths(vecs)
  n    <- max(lengths(vecs))
  vecs[lens != n] <- lapply(vecs[lens != n], rep_len, length.out = n)
  dups <- vapply(vecs, function(x){abs(x[-1] - x[-length(x)]) < tolerance},
                 logical(n - 1))
  keep <- c(TRUE, rowSums(dups) < 3L)
  list_to_df(vecs)[keep, , drop = FALSE]
}

