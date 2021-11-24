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
#'   gp_text = gpar(lineheight = c(1.2, 1.2)),
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
  hjust   <- rep_len(resolveHJust(just, hjust), n_label)

  # Match linetype to labels length
  if(length(gp_path$lty) != n_label) gp_path$lty <- rep(gp_path$lty[1], n_label)

  # Reconstitute data
  gp_text <- gp_fill_defaults(gp_text)

  if (!is.unit(x)) {
    x <- unit(x, default.units)
  }
  if (!is.unit(y)) {
    y <- unit(y, default.units)
  }

  path <- data_frame(x = x, y = y, id = rep(seq_along(id_lens), id_lens),
                     vjust = vjust, label = rep(label, id_lens),
                     lineheight = rep(gp_text$lineheight, id_lens),
                     linetype = rep(gp_path$lty, id_lens))

  path <- .groupify_linebreaks(path, flip_inverted)

  n_reps <- sapply(seq_along(unique(path$original_id)), function(i)
  {
    sub_paths <- path$original_id == unique(path$original_id)[i]
    length(unique(path$id[sub_paths]))
  })

  gp_text <- recycle_gp(gp_text, rep, times = n_reps)
  gp_path <- recycle_gp(gp_path, rep, times = n_reps)

  gTree(
    textpath = list(
      data          = path,
      hjust         = hjust,
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

  # Get gradients, angles and path lengths for each group
  path <- Map(.add_path_data, .data = split(path, path$id))

  labels <- sapply(path, function(x) x$label[1])
  # Get the actual text string positions and angles for each group
  text <- Map(.get_path_points, path = path, label = labels,
              hjust = v$hjust, gp = split_gp(v$gp_text, seq_along(labels)),
              flip_inverted = v$flip_inverted)
  text_lens <- vapply(text, nrow, integer(1))

  ## ---- Build text grob ---------------------------------------------- #

  text <- rbind_dfs(text)

  if (!all(v$gp_path$lty == 0)) {
    path <- rbind_dfs(path)

    # Get bookends by trimming paths when it intersects text
    path <- .get_surrounding_lines(path, text, v$cut_path)

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


## Split linebreaks  -----------------------------------------------

#' Split strings with linebreaks into different groups
#'
#' This function prepares the data for plotting by splitting labels
#' at line breaks and giving each its own group
#'
#' @param data A `data.frame` with at least a factor or character column
#'   called "label", integer columns called "group" and "linetype", and
#'   numeric columns called "vjust" and "lineheight".
#' @param flip_inverted If TRUE, any string where the majority of letters would
#'   be upside down along the path are inverted to improve legibility. The
#'   default is FALSE.
#'
#' @details The returned data is split into groups, one group for each
#'   segment of text such that none have line breaks. For strings that
#'   initially contained line breaks, they are broken up into different
#'   groups with different vjust values. The vjust values of each text line
#'   are centered around the originally specified vjust,
#'
#' @return A data frame containing the same column names and types as the
#'   original, but with newlines now treated as different groups.
#' @noRd
#'
#' @examples
#' xy <- data.frame(
#'   x =  1:10,
#'   y = (1:10)^2,
#'   group = 1,
#'   label = "This string \n has a line break",
#'   vjust = 0.5,
#'   linetype = 1,
#'   lineheight = 1.2
#' )
#'
#' .groupify_linebreaks(xy)
.groupify_linebreaks <- function(data, flip_inverted = FALSE)
{
    data$label <- as.character(data$label)
    data$group_min_vjust <- data$vjust
    data$group_max_vjust <- data$vjust
    data$original_id <- data$id
    data$vjust_if_inverted <- data$vjust
    multi_liners <- grepl("[\r\n]", data$label)

    if(!any(multi_liners)) return(data)

    line_breakers <- data[multi_liners,]

    pieces        <- strsplit(line_breakers$label, "[\r\n]+")
    line_breakers <- do.call(rbind, lapply(seq_along(pieces), function(i){
      n <- length(pieces[[i]])
      df <- line_breakers[rep(i, n),]
      df$label <- pieces[[i]]
      df$vjust <- (seq(n) - n)  * df$lineheight[1] +
                  df$vjust[1] * df$lineheight[1] * (n - 1) + df$vjust[1]
      df$id <- rep(df$id[1] + seq(0, 1 - 1/n, 1/n),
                      length.out = nrow(df))
      line_type <- df$linetype[1]
      df$linetype <- 0
      df$linetype[which(df$vjust <= 1 & df$vjust >= 0)] <- line_type
      if(all(df$linetype == 0)) {
        df$linetype[which.min(abs(df$vjust))] <- line_type
      }
      df$group_min_vjust <- min(df$vjust)
      df$group_max_vjust <- max(df$vjust)
      df$vjust_if_inverted <- rev(df$vjust)
      df
    }))

    data <- if(all(multi_liners)) line_breakers
            else rbind(line_breakers, data[!multi_liners,])

    data$id <- as.numeric(factor(data$id))

    data
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

