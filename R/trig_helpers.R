# ------------------------------------------------------------------------------
# This is a safe way to get the direction along a path. Since we use approx
# to interpolate angles later, we can't have any sudden transitions
# where angles "wrap around" from +180 to -180, otherwise we might
# interpolate in this transition and get letters with an angle of
# around 0. When combined with a vjust, this also makes the letters
# jump out of alignment. This little algorithm makes sure the changes
# in angle never wrap around.

.angle_from_xy <- function(x, y, degrees = FALSE, norm = FALSE)
{
  if(length(x) != length(y)) stop("x and y vectors must be the same length")

  grad       <- diff(y) / diff(x)
  first      <- atan2(diff(y), diff(x))[1]
  rads       <- atan(grad)
  diff_rads  <- if(length(rads) > 1) diff(rads) else numeric()
  diff_rads  <- ifelse(diff_rads < - pi / 2, diff_rads + pi, diff_rads)
  diff_rads  <- ifelse(diff_rads > + pi / 2, diff_rads - pi, diff_rads)
  rads       <- cumsum(c(first, diff_rads))

  if(norm) rads <- rads + pi / 2
  if(degrees) rads * 180 / pi else rads
}

# ------------------------------------------------------------------------------
# Get the cumulative length of an x, y path. The accuracy can be improved by
# setting accuracy to 1 or more, which will interpolate the points with splines
# to emulate a smooth curve through the points.

.arclength_from_xy <- function(x, y, accuracy = NA)
{
  if(length(x) != length(y)) stop("x and y must be same length")

  if(is.na(accuracy)) return(c(0, cumsum(sqrt(diff(x)^2 + diff(y)^2))))

  if(!is.numeric(accuracy) | length(accuracy) != 1 | accuracy < 0)
  {
    stop("accuracy must be a positive integer")
  }

  t <- seq_along(x)
  n <- length(x)

  new_x <- stats::spline(x ~ t, n = n + floor(accuracy) * (n - 1))
  new_y <- stats::spline(y ~ t, n = n + floor(accuracy) * (n - 1))$y

  dist <- c(0, cumsum(sqrt(diff(new_x$y)^2 + diff(new_y)^2)))

  return(dist[match(t, new_x$x)])

}

# ------------------------------------------------------------------------------
# We sometimes need to compare angles along a path, but ensure that the first
# and last elements are compared to themselves. These little utility functions
# allow a shorthand method of doing this.

.before <- function(x) x[c(1, seq_along(x))]

.after  <- function(x) x[c(seq_along(x), length(x))]


# ------------------------------------------------------------------------------
# Finds the offset path at distance d. This method effectively looks at each
# segment of the path and finds the line at distance d that runs parallel to
# it. The offset path is the set of points where adjacent offset lines meet.

.get_offset <- function(x, y, d = 0) {

  # Get angle normal to each segment of the path
  theta <- .angle_from_xy(x, y, norm = TRUE)

  # Find the angle of the lines which, when drawn at each point on the path
  # will project onto the intersections between adjacent offset segments
  theta_bisect <- (.before(theta) + .after(theta)) / 2

  # Find the distances to these intersecting points when the offset is d.
  # Since d can be a vector of distances, we need a matrix result, where
  # each column is the distance to intersections at different values of d
  offset <- outer(1/cos(theta_bisect - .after(theta)), d)

  # Calculate the actual positions of the intersection points - these are
  # our new offset paths - one matrix for x positions and one for y
  xout <- offset * cos(theta_bisect) + x
  yout <- offset * sin(theta_bisect) + y

  # Calculate arc length of the new paths: one length for each column in
  # our x and y matrices.
  arc_length <- sapply(seq(ncol(xout)), function(i) {
     .arclength_from_xy(xout[,i], yout[,i])
  })

  return(list(x = xout, y = yout, arc_length = arc_length))
}


# ------------------------------------------------------------------------------
# Finds the curvature (change in angle per change in arc length)
# This in effect finds 1/R, where R is the radius of the curve

.get_curvature <- function(x, y)
{
  dx  <- .stretch_by_one(diff(x))
  ddx <- .stretch_by_one(diff(dx))
  dy  <- .stretch_by_one(diff(y))
  ddy <- .stretch_by_one(diff(dy))
  (dx * ddy - ddx * dy) / (dx^2 + dy^2)^(3/2)
}


