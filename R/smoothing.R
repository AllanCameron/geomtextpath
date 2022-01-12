##---------------------------------------------------------------------------##
##                                                                           ##
##  smoothing.R                                                              ##
##  Part of the geomtextpath R package                                       ##
##                                                                           ##
##  Copyright (C) 2021 by Allan Cameron & Teun van den Brand                 ##
##                                                                           ##
##  Licensed under the MIT license - see https://mit-license.org             ##
##  or the LICENSE file in the project root directory                        ##
##                                                                           ##
##---------------------------------------------------------------------------##

#-------------------------------------------------------------------------------
# Spline-based smoothing for noisy paths
#-------------------------------------------------------------------------------

# Smooth both x and y as a function of path length using a spline smooth

smooth_noisy <- function(data, spar) {
  # munch path
  n           <- seq(nrow(data))
  t           <- seq(1, length(data$x), len = 1024)
  data        <- as.data.frame(lapply(data, function(x) approx(n, x, t)$y))

  # create models
  ymod        <- smooth.spline(data[c("length", "y")], spar = spar)
  xmod        <- smooth.spline(data[c("length", "x")], spar = spar)

  # Get new x and y values then calculate their length
  data$y      <- predict(ymod, data$length)$y
  data$x      <- predict(xmod, data$length)$y
  data$length <- arclength_from_xy(data$x, data$y)

  data
}


#-------------------------------------------------------------------------------
# Rounding of corners using quadratic Bezier curves
#-------------------------------------------------------------------------------

# Quadratic Bezier function

quad_bezier <- function(p0, p1, p2, t) {
  (1 - t)^2 * p0 + 2 * t * (1 - t) * p1 + t^2 * p2
}


# Return a data frame of evenly interpolated points between p1 and p2, where
# p1 and p2 are length-2 vectors representing x1, y1 and x2, y2

linear_smooth <- function(p1, p2, n) {
  x   <- seq(p1[1], p2[1], len = n)
  y   <- seq(p1[2], p2[2], len = n)
  len <- cumsum(c(0, sqrt(diff(x)^2 + diff(y)^2)))
  data.frame(x           = x,
             y           = y,
             length      = len,
             line_x      = x,
             line_y      = y,
             line_length = len)
}


# Pythagorean distance between two x-y pairs given as length-2 vectors

point_dist <- function(p0, p1) {
  sqrt((p1[1] - p0[1])^2 + (p1[2] - p0[2])^2)
}


# Produces p points around a corner given the vertex (x1, y1) and two points
# on the adjacent segments : (x0, y0) and (x2, y2)

corner_smoother <- function(p0, p1, p2, p = 20) {
  if (all(p0 == p1) && all(p1 == p2)) return(linear_smooth(p1, p1, p))
  if (all(p0 == p1)) return(linear_smooth(p1, p2, p))
  if (all(p1 == p2)) return(linear_smooth(p0, p1, p))

  lens  <- cumsum(c(0, point_dist(p0, p1), point_dist(p1, p2)))
  old_x <- approx(lens, c(p0[1], p1[1], p2[1]), seq(0, max(lens), len = p))$y
  old_y <- approx(lens, c(p0[2], p1[2], p2[2]), seq(0, max(lens), len = p))$y
  old_d <- cumsum(c(0, sqrt(diff(old_x)^2 + diff(old_y)^2)))

  t     <- seq(0, 1, len = p)
  new_x <- quad_bezier(p0[1], p1[1], p2[1], t)
  new_y <- quad_bezier(p0[2], p1[2], p2[2], t)
  new_d <- cumsum(c(0, sqrt(diff(new_x)^2 + diff(new_y)^2)))

  data.frame(x           = new_x,
             y           = new_y,
             length      = new_d,
             line_x      = old_x,
             line_y      = old_y,
             line_length = old_d)
}


# Finds the Bezier control points for corner smoothing. These are simply the
# point at the start of each segment, the point that is {radius} distance along
# the segment, the midpoint of the segment and the point that is {radius}
# distance from the other end of the segment. If the segment is shorter
# than 2 * radius, then only the end-points and midpoint are used. The points
# have to be given in the order they appear along the path.

segment_control_points <- function(x, y, len, ang, radius) {
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
  segs <- Map(f      = segment_control_points,
              x      = head(data$x, -1),
              y      = head(data$y, -1),
              len    = lens,
              ang    = angs,
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
  cps      <- find_control_points(data, radius = radius)
  sections <- lapply(seq(1, nrow(cps) - 2, 2), function(x) cps[x + 0:2, ])
  out      <- lapply(sections, function(x) corner_smoother(c(x[1, 1], x[1, 2]),
                                                           c(x[2, 1], x[2, 2]),
                                                           c(x[3, 1], x[3, 2]),
                                                           p = n))

  out       <- lapply(seq_along(out), function(x) {
    out[[x]]$segment <- x
    out[[x]]
  })

  old_lens <- numapply(out, function(x) max(x$line_length))
  out      <- Map(function(x, y) {
    x$line_length <- x$line_length + y
    return(x)
    }, x = out, y = cumsum(c(0, head(old_lens, -1))))

  new_lens <- numapply(out, function(x) max(x$length))
  out      <- Map(function(x, y) {
    x$length <- x$length + y
    return(x)
    }, x = out, y = cumsum(c(0, head(new_lens, -1))))
  out      <- rbind_dfs(out)
  out$id   <- data$id[1]
  out
}


#-------------------------------------------------------------------------------
# Co-ordinate rounding and smoothing functions
#-------------------------------------------------------------------------------


path_smoother <- function(path, text_smoothing) {

  path$x <- as_npc(path$x, "x")
  path$y <- as_npc(path$y, "y")
  text_smoothing[text_smoothing < 0]   <- 0
  path  <- split(path, path$id)
  radii <- 0.1 * text_smoothing / 200
  samps <- round(2^((100 - text_smoothing) * 0.08 + 1))
  samps[samps < 2] <- 2
  samps[samps > 1024] <- 1024
  path <- Map(smooth_corners, data = path, radius  = radii)
  path <- Map(smooth_noisy,   data = path, spar = text_smoothing / 50)
  path <- rbind_dfs(path)
  cols <- c("x", "y", "line_x", "line_y")
  path[cols] <- lapply(path[cols], unit, unit = "npc")

  path
}

