
# Preprocessing -----------------------------------------------------------

prepare_path <- function(data, label, gp, params) {

  # Deduplicate path for the safety of angle / curvature calculations
  path <- dedup_path(
    x = as_inch(data$x, "x"),
    y = as_inch(data$y, "y"),
    id = data$id
  )
  path <- split(path, path$id)

  # Convert point-like paths to proper paths
  if (any({singletons <- vapply(path, nrow, integer(1)) == 1})) {
    width <- vapply(label, function(x) max(x$xmax, na.rm = TRUE), numeric(1))
    path[singletons] <- Map(pathify,
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


#' Trim text area from path
#'
#' This function splits a path when a string is predicted to intersect with
#' the path.
#'
#' @param path A `data.frame` with at least a numeric `length` column, an
#'   integer `id` column and `vjust` column. The `id` column must match that in
#'   the `letters` argument.
#' @param letters A `data.frame` with at least a numeric `length` column and
#'   integer `id` column. The `id` column must match that in the `path`
#'   argument.
#' @param gap A single logical TRUE or FALSE which if TRUE breaks the path
#'   into two sections, one on either side of the string and if FALSE leaves the
#'   path unbroken. The default value is NA, which will break the line if the
#'   string has a vjust of between 0 and 1
#' @param vjust_lim A `numeric` of length two setting the lower and upper limits
#'   of the `vjust` column in the `path` argument, which is used to decide
#'   whether a path should be trimmed or not when `gap = NA`.
#'
#' @details We probably want the option to draw the path itself, since this will
#'   be less work for the end-user. If the `vjust` is between 0 and 1 then the
#'   path will clash with the text, so we want to remove the segment where the
#'   text is. This function will get the correct segments in either case,
#'   but it needs the whole path data *and* the calculated string data to do it.
#'
#' @return The `path` data.frame filtered for clashing segments and including
#'   a `section` column indicated it was not clipped ("all"), before ("pre") or
#'   after ("post") clipping.
#' @noRd
#'
#' @examples
#' xy <- data.frame(
#'   x =  1:10,
#'   y = (1:10)^2,
#'   id = 1
#' )
#'
#' xy <- .add_path_data(xy)
#' glyphs <- .get_path_points(xy)
#' make_gap(xy, glyphs)
make_gap <- function(path, letters, gap = NA,
                     padding = 0.15, vjust = 0.5,
                     vjust_lim = c(0, 1)) {
  padding <- as_inch(padding)
  if (is.unit(vjust)) {
    vjust <- rep_len(0.5, length(vjust))
  }

  trim <- vjust >= vjust_lim[1] & vjust <= vjust_lim[2]
  trim <- if (!is.na(gap)) rep(gap, length(trim)) else trim

  # Simplify if text isn't exactly on path
  if (!any(trim)) {
    path$section <- "all"
  } else {
    path$length <- path$length %||% .arclength_from_xy(path$x, path$y, path$id)

    # Get locations where strings start and end
    lefts <- vapply(split(letters$left, letters$id), min,
                     numeric(1), USE.NAMES = FALSE)
    rights <- vapply(split(letters$right, letters$id), max,
                     numeric(1), USE.NAMES = FALSE)
    ranges <- rbind(lefts, rights)

    # Create breathing space around letters
    path_max <- vapply(split(path$length, path$id), max,
                       numeric(1), USE.NAMES = FALSE)
    trim <- rep_len(trim, length(path_max))

    mins <- pmax(0,        ranges[1, ] - padding)
    maxs <- pmin(path_max, ranges[2, ] + padding)

    # Consider path length as following one another to avoid a loop
    sumlen <- c(0, path_max[-length(path_max)])
    sumlen <- cumsum(sumlen + seq_along(path_max) - 1)
    mins <- mins + sumlen
    maxs <- maxs + sumlen
    path$length <- path$length + sumlen[path$id]

    # Assign sections based on trimming
    section <- character(nrow(path))
    section[path$length <= mins[path$id]] <- "pre"
    section[path$length >= maxs[path$id]] <- "post"
    section[!trim[path$id]] <- "all"

    # Interpolate trimming points
    ipol <- c(mins[trim], maxs[trim])
    trim_xy <- approx_multiple(path$length, ipol, path[c("x", "y")])

    # Add trimming points to paths
    path <- data_frame(
      x  = c(path$x, trim_xy$x),
      y  = c(path$y, trim_xy$y),
      id = c(path$id, rep(which(trim), 2L)),
      length = c(path$length, ipol),
      section = c(section, rep(c("pre", "post"), each = sum(trim)))
    )[order(c(path$length, ipol)), , drop = FALSE]

    # Filter empty sections (i.e., the part where the string is)
    path <- path[path$section != "", , drop = FALSE]

    # Filter empty paths
    group <- paste0(path$id, path$section)
    len <- ave(path$length, group, FUN = function(x) {diff(range(x))})
    path <- path[len > 1e-3, ]

    # Recategorise
    sect <- ave(path$section, path$id, FUN = function(x) length(unique(x)))
    path$section[sect == "1"] <- "all"
  }
  if (!nrow(path)) {
    path$new_id <- integer()
    path$start <- logical()
    return(path)
  }

  # Get first point of individual paths
  path$new_id <- group_id(path, c("id", "section"))
  path$start  <- c(TRUE, path$new_id[-1] != path$new_id[-length(path$new_id)])

  return(path)
}

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

pathify <- function(data, hjust, angle, width,
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
tailor_arrow <- function(data, arrow) {
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

# Grob constructor --------------------------------------------------------

.add_path_grob <- function(grob, data, text, gp, params, arrow = NULL) {
  has_line <- !all((gp$lty %||% 1)  %in% c("0", "blank", NA))
  is_opaque <-!all((gp$col %||% 1) %in% c(NA, "transparent"))
  if (has_line && is_opaque) {
    data <- rbind_dfs(data)

    # Get bookends by trimming paths when it intersects text
    data <- make_gap(
      data, text,
      vjust   = params$vjust    %||% 0.5,
      gap     = params$gap      %||% NA,
      padding = params$padding  %||% 0.15
    )
    arrow <- tailor_arrow(data, arrow)

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
