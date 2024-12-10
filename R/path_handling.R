##---------------------------------------------------------------------------##
##                                                                           ##
##  path_handling.R                                                          ##
##  Part of the geomtextpath R package                                       ##
##                                                                           ##
##  Copyright (C) 2021 - 2022 by Allan Cameron & Teun van den Brand          ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

# Preprocessing -----------------------------------------------------------

prepare_path <- function(data, label, gp, params) {

  # Deduplicate path for the safety of angle / curvature calculations
  path <- dedup_path(
    x      = as_inch(data$x, "x"),
    y      = as_inch(data$y, "y"),
    id     = data$id,
    line_x = as_inch(data$line_x, "x"),
    line_y = as_inch(data$line_y, "y")
  )
  path <- split(path, path$id)

  # Convert point-like paths to proper paths

  singletons <- nrow_multi(path) == 1

  if (any(singletons)) {
    width <- numapply(label, function(x) 1.2 * max(x$xmax, na.rm = TRUE))
    if (length(params$angle) == nrow(data)) {
      index <- which(!duplicated(data$id))[singletons]
      angle <- params$angle[index]
    } else {
      angle <- params$angle
    }

    path[singletons] <- Map(pathify,
                            data    = path[singletons],
                            hjust   = params$hjust[singletons],
                            angle   = angle,
                            width   = width[singletons],
                            polar_x = list(params$polar_params$x),
                            polar_y = list(params$polar_params$y),
                            thet    = list(params$polar_params$theta))
    gp$lty[singletons] <- 0
  }
  attr(path, "gp") <- gp
  return(path)
}


# Trim text area from path

make_gap <- function(
  path,
  letters,
  gap       = NA,
  padding   = 0.05,
  vjust     = 0.5,
  vjust_lim = c(0, 1)
) {

  padding <- as_inch(padding)
  if (is.unit(vjust)) {
    vjust <- rep_len(0.5, length(vjust))
  }

  trim <- vjust >= vjust_lim[1] & vjust <= vjust_lim[2]
  trim <- if (!is.na(gap)) rep(gap, length(trim)) else trim

  # Simplify if text isn't exactly on path
  if (!any(trim)) {
    path$section <- "all"
    path$x       <- path$line_x
    path$y       <- path$line_y
    path$length  <- arclength_from_xy(path$x, path$y, path$id)
  } else {
    path$id <- discretise(path$id)

    # Find length along (smoothed) text path
    path$length <- path$length %||% arclength_from_xy(path$x, path$y, path$id)

    # Find equivalent length along actual line path
    path$line_length <- arclength_from_xy(path$line_x, path$line_y, path$id)

    # Get locations where strings start and end on (smoothed text) path
    lefts  <- gapply(letters$left,  letters$id, min, numeric(1))
    rights <- gapply(letters$right, letters$id, max, numeric(1))
    ranges <- rbind(lefts, rights)

    # Create breathing space around letters
    path_max <- gapply(path$length, path$id, max, numeric(1))
    trim     <- rep_len(trim, length(path_max))
    mins     <- pmax(0,        ranges[1, ] - padding)
    maxs     <- pmin(path_max, ranges[2, ] + padding)

    # Consider path lengths as following one another to avoid a loop
    sumlen      <- c(0, path_max[-length(path_max)])
    sumlen      <- cumsum(sumlen + seq_along(path_max) - 1)
    mins        <- mins + sumlen
    maxs        <- maxs + sumlen
    path$length <- path$length + sumlen[path$id]

    # Assign sections based on trimming
    section <- character(nrow(path))
    section[path$length <= mins[path$id]] <- "pre"
    section[path$length >= maxs[path$id]] <- "post"
    section[!trim[path$id]] <- "all"

    # Interpolate trimming points
    ipol    <- c(mins[trim], maxs[trim])
    trim_xy <- approx_multi(x    = path$length,
                            y    = path[c("line_x", "line_y", "line_length")],
                            xout = ipol)

    # Add trimming points to paths
    path <- data_frame(
      x       = c(path$line_x, trim_xy$line_x),
      y       = c(path$line_y, trim_xy$line_y),
      id      = c(path$id, rep(which(trim), 2L)),
      length  = c(path$line_length, trim_xy$line_length),
      section = c(section, rep(c("pre", "post"), each = sum(trim)))
    )[order(c(path$line_length, trim_xy$line_length)), , drop = FALSE]
    path  <- path[order(path$id), , drop = FALSE]

    # Filter empty sections (i.e., the part where the string is)
    path  <- path[path$section != "", , drop = FALSE]

    # Filter empty paths
    group <- paste0(path$id, path$section)
    len   <- ave(path$length, group, FUN = function(x) diff(range(x)))
    path  <- path[len > 1e-3, ]

    # Recategorise
    sect  <- ave(path$section, path$id, FUN = function(x) length(unique(x)))
    path$section[sect == "1"] <- "all"
  }

  if (!nrow(path)) {
    path$new_id <- integer()
    path$start  <- logical()
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
dedup_path <- function(
  x,
  y,
  id,
  line_x,
  line_y,
  tolerance = 1000 * .Machine$double.eps
) {

  vecs <- data_frame(x = interp_na(x), y = interp_na(y), id = id,
                     line_x = line_x, line_y = line_y)
  lens <- lengths(vecs)
  n    <- max(lengths(vecs))
  vecs[lens != n] <- lapply(vecs[lens != n], rep_len, length.out = n)

  dups <- vapply(vecs, function(x) abs(x[-1] - x[-length(x)]) < tolerance,
                 logical(n - 1))

  dups <- if (is.null(dim(dups))) dups[1:3] else dups[, 1:3]

  if (n > 2) {
    keep <- c(TRUE, rowSums(dups) < 3L)
  } else {
    keep <- c(TRUE, sum(dups) < 3L)
  }
  vecs <- vecs[keep, , drop = FALSE]

  vecs[complete.cases(vecs), ]
}


# Converts point-like textpaths into proper text paths.
pathify <- function(
  data,
  hjust,
  angle,
  width,
  polar_x = NULL,
  polar_y = NULL,
  thet    = NULL
) {

  angle     <- pi * angle / 180
  multi_seq <- Vectorize(seq.default)

  if (!is.null(polar_x) & !is.null(polar_y) & !is.null(thet)) {

    polar_x    <- as_inch(polar_x, "x")
    polar_y    <- as_inch(polar_y, "y")
    angle      <- angle - (as.numeric(thet == "y") * pi / 2)
    r          <- sqrt((data$x - polar_x)^2 + (data$y - polar_y)^2)
    width      <- width / r
    theta      <- atan2(data$y - polar_y, data$x - polar_x)
    theta_min  <- theta + cos(angle + pi) * width * hjust
    theta_max  <- theta + cos(angle) * width * (1 - hjust)
    r_min      <- r + sin(angle + pi) * width * hjust
    r_max      <- r + sin(angle) * width * (1 - hjust)
    theta      <- c(multi_seq(theta_min, theta_max, length.out = 100))
    r          <- c(multi_seq(r_min, r_max, length.out = 100))
    x          <- polar_x + r * cos(theta)
    y          <- polar_y + r * sin(theta)
  } else {

    xmin       <- data$x + cos(angle + pi) * width * hjust
    xmax       <- data$x + cos(angle) * width * (1 - hjust)
    ymin       <- data$y + sin(angle + pi) * width * hjust
    ymax       <- data$y + sin(angle) * width * (1 - hjust)
    x          <- c(multi_seq(xmin, xmax, length.out = 100))
    y          <- c(multi_seq(ymin, ymax, length.out = 100))
  }

  data        <- data[rep(seq(nrow(data)), each = 100), ]
  data$x      <- x
  data$y      <- y
  data$line_x <- x
  data$line_y <- y

  data
}


# This adjusts a possible arrow to not have duplicated arrowheads when a path
# is cut into two due to the path trimming.

tailor_arrow <- function(data, arrow) {

  if (is.null(arrow)) {
    return(arrow)
  }
  if (nrow(data) == 0 || ncol(data) == 0) {
    return(data)
  }

  keep  <- !duplicated(data$new_id)
  sides <- data$section[keep]
  id    <- data$id[keep]

  # Have arrow match the length of the groups
  arrow[] <- lapply(arrow, function(x) {
    x[pmin(id, length(x))]
  })
  ends  <- arrow$ends
  lens  <- arrow$length

  # Ends are 1 = "first", 2 = "last", 3 = "both".
  # We 'hide' an arrow by setting a zero length
  lens[ends == 2 & sides == "pre"]  <- unit(0, "pt")
  lens[ends == 1 & sides == "post"] <- unit(0, "pt")
  ends[ends == 3 & sides == "pre"]  <- 1L
  ends[ends == 3 & sides == "post"] <- 2L
  arrow$ends   <- ends
  arrow$length <- lens
  arrow
}


# Grob constructor --------------------------------------------------------

add_path_grob <- function(grob, data, text, gp, params, arrow = NULL) {

  has_line  <- !all((gp$lty %||% 1)  %in% c("0", "blank", NA))
  is_opaque <- !all((gp$col %||% 1) %in% c(NA, "transparent"))
  if (has_line && is_opaque) {
    data <- rbind_dfs(data)

    # Get bookends by trimming paths when it intersects text
    data <- make_gap(
      data, text,
      vjust   = params$vjust    %||% 0.5,
      gap     = params$gap      %||% NA,
      padding = params$padding  %||% 0.05
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
