
# ------------------------------------------------------------------------------
# Often we need the derivative or gradient to match the length of the input
# vector. This does it via a simple interpolation along the input vector

.stretch_by_one <- function(vec)
{
  n <- length(vec)

  if(n == 1)
    rep(vec, 2)
  else
    approx(seq(n), vec, seq(1, n, length.out = n + 1))$y
}

# ------------------------------------------------------------------------------
# Given x, y co-ordinates, get the value of dy / dx (i.e. the gradient)

.derivative <- function(x, y, stretch = TRUE)
{
  n <- length(x)

  if(n != length(y)) stop("x and y must be same length")

  result <- diff(y) / diff(x)

  if(!stretch) result else .stretch_by_one(result)
}

# ------------------------------------------------------------------------------
# This is a safe way to get the direction along a path. Since we use approx
# to interpolate angles later, we can't have any sudden transitions
# where angles "wrap around" from +180 to -180, otherwise we might
# interpolate in this transition and get letters with an angle of
# around 0. When combined with a vjust, this also makes the letters
# jump out of alignment. This little algorithm makes sure the changes
# in angle never wrap around.

.angle_from_xy <- function(x, y, degrees = FALSE, stretch = FALSE, norm = FALSE)
{
  rads <- atan(.derivative(x, y, stretch = stretch))

  if (length(rads) > 1) {
    diff_rads <- diff(rads)
    diff_rads <- ifelse(diff_rads < - pi / 2, diff_rads + pi, diff_rads)
    diff_rads <- ifelse(diff_rads > + pi / 2, diff_rads - pi, diff_rads)
    rads <- cumsum(c(rads[1], diff_rads))
  }
  else {
    diff_rads <- c(0, 0)
    rads <- rep(rads, 2)
  }
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


  if(!is.na(accuracy))
  {
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
  else
  {
    return(c(0, cumsum(sqrt(diff(x)^2 + diff(y)^2))))
  }
}

.before <- function(x) x[c(1, seq_along(x))]

.after  <- function(x) x[c(seq_along(x), length(x))]

# The norms of a path tell us the norm of each *segment*. We want to know the
# norm at each *point*, which is just the mean angle at the two adjacent
# segments. Each end point simply gets the angle of its adjacent segment.
.bisect_angles <- function(theta)
{
  (.before(theta) + .after(theta)) / 2
}

.bisector_offset <- function(theta, d) {

  # Calculate x position at angle bisector
  xx <- cos(theta)
  xx <- .before(xx) * .after(xx)

  # Calculate y position at angle bisector
  yy <- sin(theta)
  yy <- .before(yy) * .after(yy)

  # Find appropriate length along bisector
  outer(sqrt(2) / sqrt(1 + xx + yy), d)
}



.get_offset <- function(x, y, d = 0) {

  norm_theta <- .angle_from_xy(x, y, norm = TRUE)

  # Calculate angle bisector
  bisector_angles <- .bisect_angles(norm_theta)

  offset <- .bisector_offset(norm_theta, d)

  # Project new points at the bisector
  xout <- offset * cos(bisector_angles) + x
  yout <- offset * sin(bisector_angles) + y

  # Calculate arc length
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


# ------------------------------------------------------------------------------
# Finds the length of each part of the offset path that projects onto the
# original path
.length_adjust_by_curvature <- function(x, y, offset)
{
  curvature         <- .get_curvature(x, y)
  radius            <- 1 / curvature
  radius[radius > 1e6] <- 1e6
  radius[radius < -1e6] <- -1e6
  length_correction <-  (radius + offset) / radius

  effective_length  <- c(0, diff(.arclength_from_xy(x, y))) * length_correction

  cumsum(effective_length)
}

