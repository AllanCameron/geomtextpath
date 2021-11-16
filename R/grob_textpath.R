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
  vjust <- rep_len(resolveVJust(just, vjust), n_label)
  hjust <- rep_len(resolveHJust(just, hjust), n_label)

  # Reconstitute data
  path <- data.frame(
    x = x, y = y,
    group    = rep(seq_along(id_lens), id_lens),
    label    = rep(label, id_lens),
    fontface = rep(gp_text$fontface %||% rep_len(1, n_label), id_lens),
    hjust    = rep(hjust, id_lens),
    vjust    = rep(vjust, id_lens),
    size     = rep(gp_text$fontsize %||% rep_len(3.88 * .pt, n_label), id_lens)
  )

  ## ---- Data manipulation -------------------------------------------- #

  # Get gradients, angles and path lengths for each group
  path <- lapply(split(path, path$group), .add_path_data)

  # Get the actual text string positions and angles for each group
  strings <- do.call(rbind.data.frame, c(lapply(path, .get_path_points),
                                         make.row.names = FALSE))
  string_lens <- rle(strings$group)$lengths
  path <- do.call(rbind.data.frame, c(path, make.row.names = FALSE))

  # Get bookends by trimming paths when it intersects text
  path <- .get_surrounding_lines(path, strings)

  # Get first point of individual paths for recycling
  path_id <- paste0(path$group, "&", path$section)
  path_id <- match(path_id, unique(path_id))
  path_start   <- c(TRUE, path_id[-1] != path_id[-length(path_id)])

  # Recycle graphical parameters to match lengths of strings / path
  gp_text <- recycle_gp(gp_text, rep, times = string_lens)
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
      label = strings$label,
      x = strings$x, y = strings$y, rot = strings$angle,
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

# Similar to rlang::`%||%` or utils:::`%||%`
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
