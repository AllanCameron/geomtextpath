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
#'   id = rep(1:2, each = length(t))
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
  gp_text = gpar(),
  gp_path = gpar(),
  default.units = "npc",
  name = NULL,
  vp = NULL
) {
  n_label <- length(label)
  id_lens <- rle(id)$lengths

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
  vjust   <- rep_len(resolveVJust(just, vjust), n_label)
  hjust   <- rep_len(resolveHJust(just, hjust), n_label)

  # Reconstitute data
  gp_text <- gp_fill_defaults(gp_text)
  path <- data.frame(
    x = x, y = y,
    group    = rep(seq_along(id_lens), id_lens),
    label    = rep(label, id_lens),
    fontface = rep(rep_len(gp_text$font,           n_label), id_lens),
    family   = rep(rep_len(gp_text$fontfamily,     n_label), id_lens),
    size     = rep(rep_len(gp_text$fontsize / .pt, n_label), id_lens),
    lineheight = rep(rep_len(gp_text$lineheight,   n_label), id_lens),
    hjust    = rep(hjust, id_lens),
    vjust    = rep(vjust, id_lens)
  )

  ## ---- Data manipulation -------------------------------------------- #

  # TODO: Make sure that helper functions accept static label parameters

  # Get gradients, angles and path lengths for each group
  path <- Map(.add_path_data, .data = split(path, path$group), vjust = vjust)

  # Get the actual text string positions and angles for each group
  text <- Map(.get_path_points, path = path, label = label, hjust = hjust,
                 gp = split_gp(gp_text, seq_along(label)))
  text_lens <- vapply(text, nrow, integer(1))

  text <- do.call(rbind.data.frame, c(text, make.row.names = FALSE))
  path <- do.call(rbind.data.frame, c(path, make.row.names = FALSE))

  # Get bookends by trimming paths when it intersects text
  path <- .get_surrounding_lines(path, text)

  # Get first point of individual paths for recycling
  path_id <- paste0(path$group, "&", path$section)
  path_id <- match(path_id, unique(path_id))
  path_start   <- c(TRUE, path_id[-1] != path_id[-length(path_id)])

  # Recycle graphical parameters to match lengths of strings / path
  gp_text <- recycle_gp(gp_text, rep, times = text_lens)
  gp_path <- recycle_gp(gp_path, `[`, i = path$group[path_start])

  # ---- Grob writing --------------------------------------------------- #

  my_tree <- gTree(name = name, vp = vp)

  my_tree <- addGrob(
    my_tree, polylineGrob(
      x = path$x, y = path$y, id = path_id, gp = gp_path,
      default.units = default.units
    )
  )

  my_tree <- addGrob(
    my_tree, textGrob(
      label = text$label,
      x = text$x, y = text$y, rot = text$angle,
      vjust = vjust, hjust = 0.5, gp = gp_text,
      default.units = default.units
    )
  )

  my_tree
}

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

# Similar to rlang::`%||%` or utils:::`%||%`
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
