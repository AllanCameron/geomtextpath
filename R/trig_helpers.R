##---------------------------------------------------------------------------##
##                                                                           ##
##  trige_helpers.R                                                          ##
##  Part of the geomtextpath R package                                       ##
##                                                                           ##
##  Copyright (C) 2021-2022 by Allan Cameron & Teun van den Brand            ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

# Constants --------------------------------------------------------------------

.rad2deg <- 180 / pi
.deg2rad <- pi / 180
.halfpi  <- pi / 2


# Checker ----------------------------------------------------------------------

# We can only define paths if they have two or more valid numeric points, and no
# path can contain any infinite values

check_xy <- function(x, y) {

  stopifnot(
    "x and y must be the same length" =
      (n <- length(x)) == length(y),
    "x and y must both be numeric" =
      (is.numeric(x) & is.numeric(y)),
    "x and y must be length 2 or more" =
      n > 1,
    "x and y must not contain infinite points" =
      all(is.finite(x[!is.na(x)]) & is.finite(y[!is.na(y)]))
  )
}


# Angles -----------------------------------------------------------------------

# This is a safe way to get the direction along a path (or its norm)
# in radians or degrees, whether in grid units or bare numbers.

angle_from_xy <- function(x, y, degrees = FALSE, norm = FALSE) {

  x <- as_npc(x, "x")
  y <- as_npc(y, "y")

  check_xy(x, y)

  x <- interp_na(x)
  y <- interp_na(y)

  rads <- atan2(diff(y), diff(x))
  rads <- rads + .halfpi * as.numeric(norm)
  rads <- rads * .rad2deg ^ (as.numeric(degrees))

  rads
}


# Arclength --------------------------------------------------------------------

# Get the cumulative length of an x, y path safely, by group (id) if needed

arclength_from_xy <- function(x, y, id = NULL) {

  # Handle length-1 paths (including NAs) correctly
  if (length(x) == 1 || length(y) == 1) return(0 * x[1] + 0 * y[1])

  x <- as_npc(x, "x")
  y <- as_npc(y, "y")

  check_xy(x, y)

  x <- interp_na(x)
  y <- interp_na(y)

  if (is.null(id)) id <- rep(seq_len(NCOL(x)), each = NROW(x))

  start <- run_start(id)
  dist  <- sqrt(diff(x)^2 + diff(y)^2)

  # Should ideally be something like vctrs::vec_c(0, dist)
  if (is.null(dim(dist))) {
    dist <- c(0, dist)
  } else {
    dist <- rbind(0, dist)
  }
  dist[start] <- 0

  ave(dist, id, FUN = cumsum)
}


# Before / After ---------------------------------------------------------------

# We sometimes need to compare angles along a path, but ensure that the first
# and last elements are compared to themselves. These little utility functions
# allow a shorthand method of doing this.

before <- function(x) {

  if (length(x) == 0) x else x[c(1, seq_along(x))]
}


after <- function(x) {

  if (length(x) == 0) x else x[c(seq_along(x), length(x))]
}


# Bisect offset ----------------------------------------------------------------
# Finds the offset path at distance d. This method effectively looks at each
# segment of the path and finds the line at distance d that runs parallel to
# it. The offset path is the set of points where adjacent offset lines meet.

get_offset <- function(x, y, d = 0) {

  # Get angle normal to each segment of the path
  theta <- angle_from_xy(x, y, norm = TRUE)

  # Find the angle of the lines which, when drawn at each point on the path
  # will project onto the intersections between adjacent offset segments
  theta_bisect <- (before(theta) + after(theta)) / 2

  # Find the distances to these intersecting points when the offset is d.
  # Since d can be a vector of distances, we need a matrix result, where
  # each column is the distance to intersections at different values of d
  offset <- outer(1 / cos(theta_bisect - after(theta)), d)

  # Calculate the actual positions of the intersection points - these are
  # our new offset paths - one matrix for x positions and one for y
  xout <- offset * cos(theta_bisect) + x
  yout <- offset * sin(theta_bisect) + y

  # Calculate arc length of the new paths: one length for each column in
  # our x and y matrices.
  arc_length <- arclength_from_xy(xout, yout)

  return(list(x = xout, y = yout, arc_length = arc_length))
}


# Produces kernel-based smoothing of paths for use with straight labels

get_smooth_offset <- function(x, y, d, width = 0.02) {

  dist <- arclength_from_xy(x, y)
  sd   <- max(dist) * width

  x <- sapply(dist, function(i) {
    dn <- dnorm(dist, mean = i, sd = sd)
    sum(x * dn / sum(dn))
  })

  y <- sapply(dist, function(i) {
    dn <- dnorm(dist, mean = i, sd = sd)
    sum(y * dn / sum(dn))
  })

  get_offset(x, y, d)
}


# Curvature --------------------------------------------------------------------

# Finds the curvature (change in angle per change in arc length)
# This in effect finds 1/R, where R is the radius of the curve

get_curvature <- function(x, y) {

  if (length(x) < 3) return(rep(0, length(x)))

  dx   <- diff(x)
  ddx  <- diff(dx)
  dy   <- diff(y)
  ddy  <- diff(dy)
  dx   <- head(dx, -1)
  dy   <- head(dy, -1)
  curv <- (dx * ddy - ddx * dy) / (dx^2 + dy^2)^1.5

  # Duplicate first and last entries, since these are the best estimates
  # of the curvature at these points, which is otherwise undefined.
  before(after(curv))
}


# Detects whether the distance d ever exceeds curvature of an [x, y] path

exceeds_curvature <- function(x, y, d, tolerance = 0.1) {

  curve_radius <- 1 / get_curvature(x, y)
  as.numeric(apply(outer(curve_radius, d,
        FUN = function(a, b) {
          (abs(a) < abs(b)) & (sign(a) == sign(b))
        }), 1, any))
}


# A rollmean that returns the same length vector as was input

safe_rollmean <- function(vec, k = 10) {

  if (k < 2) return(vec)
  mat <- sapply(seq_along(vec) - k / 2, function(x) x + 0:(k - 1))
  mat[mat < 1] <- 1
  mat[mat > length(vec)] <- length(vec)
  mat <- round(mat)
  dm  <- dim(mat)
  mat <- vec[mat]
  return(colMeans(`dim<-`(mat, dm)))
}


# Returns the index of the flattest point of an x, y path.

which.min_curvature <- function(x, y, k = 10) {

  len       <- arclength_from_xy(x, y)
  len       <- len / max(len)
  curv      <- abs(get_curvature(x, y))
  mean_curv <- safe_rollmean(curv, k)

  which.min(abs(mean_curv) - min(abs(mean_curv)))
}


# Rounding corners -------------------------------------------------------------

# Checks whether path contains any angles greater than 12 degrees, which is an
# approximate value beyond which paths appear cornered or angular.

has_corners <- function(x, y) {

  angles <- angle_from_xy(x, y, degrees = TRUE)
  any(abs(diff(angles)) > 12)
}


# Rounds corners of a closed x,y polygon with some radius at positions marked
# by the 'at' variable by making 'n' new points on a circle. Doesn't round first
# or last point.

round_corners <- function(x, y, radius, at, n = 10) {

  len  <- arclength_from_xy(x, y)

  # Find surrounding points around corners at radius distance
  pts <- len[at]
  pts <- cbind(pts + radius, pts, pts - radius)

  # Find which points are inside those radii, to delete later
  drop <- outer(len, pts[, 3], "<=")
  drop <- drop | outer(len, pts[, 1], ">=")
  drop <- which(rowSums(drop) != nrow(pts))

  # Find appropriate x/y for surrounding points
  proj <- approx_multi(x = len, y = cbind(x, y), xout = as.vector(pts))
  xx   <- `dim<-`(proj[, 1], dim(pts))
  yy   <- `dim<-`(proj[, 2], dim(pts))

  # Find center-points of circles at corners
  ang   <- atan2(yy[, 2] - yy[, 1], xx[, 2] - xx[, 1])
  cen_x <- cos(ang - .halfpi) * radius + xx[, 1]
  cen_y <- sin(ang - .halfpi) * radius + yy[, 1]

  # Calculate angle from center to surrounding points
  ang_start <- atan2(yy[, 1] - cen_y, xx[, 1] - cen_x)
  ang_end   <- atan2(yy[, 3] - cen_y, xx[, 3] - cen_x)
  ang_delta <- ang_end - ang_start

  # Correct angle difference for difference in phases
  ang_delta <- ang_delta - (2 * pi) * sign(ang_delta) * (abs(ang_delta) > pi)

  # Sequence from ang_start to ang_end (but vectorised)
  weight  <- seq(0, 1, length.out = n)
  ang_seq <- outer(ang_delta, weight) + ang_start

  # Find points on circle pieces
  new_x <- cos(ang_seq) * radius + cen_x
  new_y <- sin(ang_seq) * radius + cen_y

  # Find positions where new points should be inserted
  arcs <- outer(pts[, 3] - pts[, 1], weight) + pts[, 1]
  ord  <- setdiff(order(c(len, as.vector(arcs))), drop)

  # Insert new positions
  list(
    x = c(x, new_x)[ord],
    y = c(y, new_y)[ord]
  )
}
