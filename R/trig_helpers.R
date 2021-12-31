# Constants ---------------------------------------------------------------

.rad2deg <- 180 / pi
.deg2rad <- pi / 180
.halfpi  <- pi /2


# Checkers ----------------------------------------------------------------

# We can only define paths if they have two or more valid numeric points, and no
# path can contain any infinite values

check_xy <- function(x, y) {

  stopifnot(

    "x and y must be the same length" = {
      (n <- length(x)) == length(y)
    },

    "x and y must both be numeric" = {
      (is.numeric(x) & is.numeric(y))
    },

    "x and y must be length 2 or more" = {
      n > 1
    },

    "x and y must not contain infinite points" = {
      all(is.finite(x[!is.na(x)]) & is.finite(y[!is.na(y)]))
    }
  )
}

# Angles ------------------------------------------------------------------

# This is a safe way to get the direction along a path (or its norm)
# in radians or degrees, whether in grid units or bare numbers.

angle_from_xy <- function(x, y, degrees = FALSE, norm = FALSE) {

  # Allow x and y to be grid units
  if (grid::is.unit(x)) x <- grid::convertUnit(x, "npc", valueOnly = TRUE)
  if (grid::is.unit(y)) y <- grid::convertUnit(y, "npc", valueOnly = TRUE)

  check_xy(x, y)

  x <- interp_na(x)
  y <- interp_na(y)

  rads <- atan2(diff(y), diff(x))

  if (norm) rads <- rads + .halfpi
  if (degrees) rads * .rad2deg else rads
}

# Arclength ---------------------------------------------------------------

# Get the cumulative length of an x, y path safely

arclength_from_xy <- function(x, y, id = NULL) {

  # Handle length-1 paths correctly
  if (length(x) == 1) return(0 * x + 0 * y[1])

  # Allow x and y to be grid units
  if (grid::is.unit(x)) x <- grid::convertUnit(x, "npc", valueOnly = TRUE)
  if (grid::is.unit(y)) y <- grid::convertUnit(y, "npc", valueOnly = TRUE)

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

# Before / After ----------------------------------------------------------

# We sometimes need to compare angles along a path, but ensure that the first
# and last elements are compared to themselves. These little utility functions
# allow a shorthand method of doing this.

before <- function(x) {

  if (length(x) == 0) x else x[c(1, seq_along(x))]
}

after <- function(x) {

  if (length(x) == 0) x else x[c(seq_along(x), length(x))]
}

# Some features of a path are associated with its points, such as its x and y
# coordinates, whereas others are associated with the *segments* joining the
# points, such as the slope or angle. Still others can only really be
# defined by a change in two adjacent segments, such as curvature. For a path
# with n points, there will therefore be some features that have length n,
# some that have length n - 1, and some that have length n - 2. This is a
# problem if we want to have these features all defined at the same points, as
# would be possible with a truly differentiable curve. One way to get round this
# for n - 1 features is to associate the two end points with the two end
# segments, and all intervening points with the mean of their two adjacent
# segments.

average_segments_at_points <- function(x) {

  (before(x) + after(x)) / 2
}

# Bisect offset -----------------------------------------------------------

# Finds the offset path at distance d. This method effectively looks at each
# segment of the path and finds the line at distance d that runs parallel to
# it. The offset path is the set of points where adjacent offset lines meet.

get_offset <- function(x, y, d = 0) {

  # Get angle normal to each segment of the path
  theta <- angle_from_xy(x, y, norm = TRUE)

  # Find the angle of the lines which, when drawn at each point on the path
  # will project onto the intersections between adjacent offset segments
  theta_bisect <- average_segments_at_points(theta)

  # Find the distances to these intersecting points when the offset is d.
  # Since d can be a vector of distances, we need a matrix result, where
  # each column is the distance to intersections at different values of d
  offset <- outer(1/cos(theta_bisect - after(theta)), d)

  # Calculate the actual positions of the intersection points - these are
  # our new offset paths - one matrix for x positions and one for y
  xout <- offset * cos(theta_bisect) + x
  yout <- offset * sin(theta_bisect) + y

  # Calculate arc length of the new paths: one length for each column in
  # our x and y matrices.
  arc_length <- arclength_from_xy(xout, yout)

  return(list(x = xout, y = yout, arc_length = arc_length))
}

# Smooth offset -----------------------------------------------------------

get_smooth_offset <- function(x, y, d, width = 0.02) {

  dist <- arclength_from_xy(x, y)
  sd   <- max(dist) * width

  x <- sapply(dist, function(i) {
    dn <- dnorm(dist, mean = i, sd = sd)
    sum(x * dn/sum(dn))
  })

  y <- sapply(dist, function(i) {
    dn <- dnorm(dist, mean = i, sd = sd)
    sum(y * dn/sum(dn))
  })

  get_offset(x, y, d)
}

# Curvature ---------------------------------------------------------------

# Finds the curvature (change in angle per change in arc length)
# This in effect finds 1/R, where R is the radius of the curve

get_curvature <- function(x, y)
{
  if (length(x) < 3) return(rep(0, length(x)))

  dx  <- diff(x)
  ddx <- diff(dx)
  dy  <- diff(y)
  ddy <- diff(dy)
  dx  <- head(dx, -1)
  dy  <- head(dy, -1)

  curv <- (dx * ddy - ddx * dy) / (dx^2 + dy^2)^(3/2)

  # Duplicate first and last entries, since these are the best estimates
  # of the curvature at these points, which is otherwise undefined.
  before(after(curv))
}

exceeds_curvature <- function(x, y, d, tolerance = 0.1)
{
  curve_radius <- 1 / get_curvature(x, y)
  as.numeric(apply(outer(curve_radius, d,
        FUN = function(a, b) {
          (abs(a) < abs(b)) & (sign(a) == sign(b))
        }), 1, any))
}

safe_rollmean <- function(vec, k = 10) {

  if (k < 2) return(vec)

  mat <- sapply(-(k/2 - 1):(length(vec) - k/2), function(x) x + 0:(k - 1))
  mat[mat < 1] <- 1
  mat[mat > length(vec)] <- length(vec)
  mat <- round(mat)
  dm <- dim(mat)
  mat <- vec[mat]
  return(colMeans(`dim<-`(mat, dm)))
}


which.min_curvature <- function(x, y, k = 10) {
  len <- arclength_from_xy(x, y)
  len <- len / max(len)
  curv <- abs(get_curvature(x, y))
  mean_curv <- safe_rollmean(curv, k)
  which.min(abs(mean_curv) - min(abs(mean_curv)))
}

# Rounding corners --------------------------------------------------------

# Rounds corners of a closed x,y polygon with some radius at positions marked
# by the 'at' variable by making 'n' new points on a circle. Doesn't round first
# or last point.
# Example:
# # Make rectangle
# x <- c(0, 1, 1, 0, 0)
# y <- c(0, 0, 1, 1, 0)
# plot(round_corners(x, y, 0.2, c(2, 4)), type = 'l')
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
  proj <- approx_multiple(len, as.vector(pts), cbind(x, y))
  xx <- `dim<-`(proj[, 1], dim(pts))
  yy <- `dim<-`(proj[, 2], dim(pts))

  # Find center-points of circles at corners
  ang <- atan2(yy[, 2] - yy[, 1], xx[, 2] - xx[, 1])
  cen_x <- cos(ang - .halfpi) * radius + xx[, 1]
  cen_y <- sin(ang - .halfpi) * radius + yy[, 1]

  # Calculate angle from center to surrounding points
  ang_start <- atan2(yy[, 1] - cen_y, xx[, 1] - cen_x)
  ang_end   <- atan2(yy[, 3] - cen_y, xx[, 3] - cen_x)
  ang_delta <- ang_end - ang_start

  # Correct angle difference for difference in phases
  ang_delta <- ang_delta - (2 * pi) * sign(ang_delta) * (abs(ang_delta) > pi)

  # Sequence from ang_start to ang_end (but vectorised)
  weight <- seq(0, 1, length.out = n)
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


# Simple test for whether a path has "corners"
has_corners <- function(x, y) {

  angles <- angle_from_xy(x, y, degrees = TRUE)
  any(abs(diff(angles)) > 12)
}


#-------------------------------------------------------------------------------
# Spline-based smoothing for noisy paths
#-------------------------------------------------------------------------------

# Simple single-variable spline interpolation
spline_smooth <- function(x, n = 4) {

  stopifnot("length must be 2 or more for smoothing" = length(x[!is.na(x)]) > 1)
  t <- seq_along(x)
  spline(t, x, n = n * length(x))$y
}

# Chunk the path into n parts and get the centroid of each chunk
sample_path <- function(data, n = 50) {

  samp_rows <- seq(1, nrow(data), round(nrow(data)/n))
  if(tail(samp_rows, 1) != nrow(data)) samp_rows <- c(samp_rows, nrow(data))

  x <- spline_smooth(data$x[samp_rows], n = nrow(data) / n)
  y <- spline_smooth(data$y[samp_rows], n = nrow(data) / n)

  data$x <- approx(seq_along(x), x, seq(1, length(x), len = 1000))$y
  data$y <- approx(seq_along(y), y, seq(1, length(y), len = 1000))$y

  data$length <- arclength_from_xy(data$x, data$y, data$id)

  data

}

# Spline smooth the centroids of a path split into n chunks
smooth_noisy <- function(data, samples = 50) {

  # data is a data frame with an accurate x, y, length and mapped values,
  # representing a *single* corner-smoothed path and the original path
  # We will munch the path into 1000 pieces to start with

  n <- seq(nrow(data))
  t <- seq(1, length(data$x), len = 1000)
  data <- as.data.frame(lapply(data, function(x) approx(n, x, t)$y))

  # Now we sample the path regularly along its length
  sample_path(data, n = samples)

}


#-------------------------------------------------------------------------------
# Rounding of corners using quadratic Bezier curves
#-------------------------------------------------------------------------------

# Quadratic Bezier function
quad_bezier <- function(p0, p1, p2, t) {

  (1 - t)^2 * p0 + 2 * t * (1 - t) * p1 + t^2 * p2
}

linear_smooth <- function(p1, p2, n) {

  x <- seq(p1[1], p2[1], len = n)
  y <- seq(p1[2], p2[2], len = n)
  len <- cumsum(c(0, sqrt(diff(x)^2 + diff(y)^2)))
  data.frame(x = x, y = y, length = len, line_x = x, line_y = y,
             line_length = len)
}

# Produces p points around a corner given the vertex (x1, y1) and two points
# on the adjacent segment : (x0, y0) and (x2, y2)
corner_smoother <- function(p0, p1, p2, p = 20) {

  if(all(p0 == p1) && all(p1 == p2)) {
    return(data.frame(x = rep(p0[1], p), y = rep(p0[2], p),
                      length = rep(0, p), line_x = rep(p0[1], p),
                      line_y = rep(p0[2], p), line_length = rep(0, p)))
  }

  if(all(p0 == p1)) return(linear_smooth(p1, p2, p))
  if(all(p1 == p2)) return(linear_smooth(p0, p1, p))

  lens <- cumsum(c(0, sqrt((p1[1] - p0[1])^2 + (p1[2] - p0[2])^2),
                   sqrt((p2[1] - p1[1])^2 + (p2[2] - p1[2])^2)))

  old_x <- approx(lens, c(p0[1], p1[1], p2[1]), seq(0, max(lens), len = p))$y
  old_y <- approx(lens, c(p0[2], p1[2], p2[2]), seq(0, max(lens), len = p))$y
  old_d <- cumsum(c(0, sqrt(diff(old_x)^2 + diff(old_y)^2)))

  t <- seq(0, 1, len = p)
  new_x <- quad_bezier(p0[1], p1[1], p2[1], t)
  new_y <- quad_bezier(p0[2], p1[2], p2[2], t)
  new_d <- cumsum(c(0, sqrt(diff(new_x)^2 + diff(new_y)^2)))

  data.frame(x = new_x, y = new_y, length = new_d,
             line_x = old_x, line_y = old_y, line_length = old_d)
}

segment_control_points <- function(x, y, len, ang, n, radius) {

    if (len < 2 * radius) {
      cbind(c(x, x + 0.5 * cos(ang) * len),
            c(y, y + 0.5 * sin(ang) * len))
    } else {
      cbind(x + cos(ang) * c(0, radius, 0.5 * len, len - radius),
            y + sin(ang) * c(0, radius, 0.5 * len, len - radius))
    }
}

# Takes a path and a corner radius to find the control points on the path
# that will give Bezier curves with the given radius
find_control_points <- function(data, radius = 0.1) {

  lens <- diff(arclength_from_xy(data$x, data$y))
  angs <- angle_from_xy(data$x, data$y)

  segs <- Map(segment_control_points,
              x   = head(data$x, -1),
              y   = head(data$y, -1),
              len = lens,
              ang = angs,
              radius = radius)

  segs <- do.call(rbind, segs)

  rbind(segs[1, ],
        segs,
        cbind(c(tail(data$x, 1), tail(data$x, 1)),
              c(tail(data$y, 1), tail(data$y, 1))
              )
        )
}

# Co-ordinates the above functions to generate a Bezier-smoothed curve
smooth_corners <- function(data, n = 20, radius = 0.1) {

  cps <- find_control_points(data, radius = radius)
  sections <- lapply(seq(1, nrow(cps) - 2, 2), function(x) cps[x + 0:2,])
  out <- lapply(sections, function(x) corner_smoother(c(x[1, 1], x[1, 2]),
                                                      c(x[2, 1], x[2, 2]),
                                                      c(x[3, 1], x[3, 2]),
                                                      p = n))

  out <- lapply(seq_along(out), function(x) {out[[x]]$segment <- x; out[[x]]})
  old_lens <- vapply(out, FUN = function(x) max(x$line_length),
                     FUN.VALUE = numeric(1))
  out <- Map(function(x, y) {x$line_length <- x$line_length + y; x},
             x = out, y = cumsum(c(0, head(old_lens, -1))))
  new_lens <- vapply(out, FUN = function(x) max(x$length),
                     FUN.VALUE = numeric(1))
  out <- Map(function(x, y) {x$length <- x$length + y; x},
             x = out, y = cumsum(c(0, head(new_lens, -1))))

  out <- do.call(rbind, out)

  out$id <- data$id[1]
  out
}


# Applies the smoothing functions to a data frame consisting of x, y and id
path_smoother <- function(path, text_smoothing) {

  if (grid::is.unit(path$x)) {
    path$x <- grid::convertUnit(path$x, "npc", valueOnly = TRUE)
  }
  if (grid::is.unit(path$y)) {
    path$y <- grid::convertUnit(path$y, "npc", valueOnly = TRUE)
  }

  text_smoothing[text_smoothing > 100] <- 100
  text_smoothing[text_smoothing < 0]   <- 0
  path  <- split(path, path$id)
  lens  <- vapply(path, FUN = nrow, FUN.VALUE = numeric(1))
  radii <- 0.1 * text_smoothing / 100
  samps <- round(400 * (100 - text_smoothing) / 100)
  samps[samps < 3] <- 3
  path <- Map(smooth_corners, data = path, radius = radii)
  path <- Map(smooth_noisy, data = path, samples = samps)
  path <- do.call(rbind, path)

  path$x <- grid::unit(path$x, "npc")
  path$y <- grid::unit(path$y, "npc")
  path$line_x <- grid::unit(path$line_x, "npc")
  path$line_y <- grid::unit(path$line_y, "npc")

  path

}
