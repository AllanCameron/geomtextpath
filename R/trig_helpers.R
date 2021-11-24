
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

.path_angle_at_xy <- function(x, y, degrees = TRUE)
{
  rads <- atan(.derivative(x, y))

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

# ------------------------------------------------------------------------------
# Finds the xy co-ordinates at offset d from original x, y coordinates
.get_offset_path <- function(x, y, d)
{
   angle <- .path_angle_at_xy(x, y, degrees = FALSE) + pi / 2

   data.frame(x = d * cos(angle) + x, y = d * sin(angle) + y)
}

calc_offset <- function(x, y, d = 0) {
  n  <- length(x)
  dx <- diff(x)
  dy <- diff(y)
  ang  <- atan(dy / dx)
  dang <- diff(ang)
  dang <- ifelse(dang < - pi / 2, dang + pi, dang)
  dang <- ifelse(dang > + pi / 2, dang - pi, dang)
  ang <- atan2(dy[1], dx[1]) # Never reorient first angle

  # Get orthogonal angles
  ang <- cumsum(c(ang[1], dang)) + pi / 2

  # Left / right aligned indices
  before <- c(1L, seq_along(ang))
  after  <- c(seq_along(ang), length(ang))

  # Calculate angle bisector
  bis <- (ang[before] + ang[after])/2

  # Calculate x position at angle bisector
  xx <- cos(ang)
  xx <- xx[before] * xx[after]

  # Calculate y position at angle bisector
  yy <- sin(ang)
  yy <- yy[before] * yy[after]

  # Find appropriate length along bisector
  len <- outer(sqrt(2) / sqrt(1 + xx + yy), d)

  # Project new points at the bisector
  xout <- len * cos(bis) + x
  yout <- len * sin(bis) + y

  # Calculate arc length
  arc_length <- rbind(0, sqrt(diff(xout)^2 + diff(yout)^2))
  arc_length <- apply(arc_length, 2, cumsum)

  return(list(x = xout, y = yout, arc_length = arc_length))
}

# ------------------------------------------------------------------------------
# Finds the proportional change in path length
.length_adjustment_at_d <- function(x, y, d, accuracy = 0)
{
  offset_df <- .get_offset_path(x, y, d)
  .arclength_from_xy(offset_df$x, offset_df$y, accuracy)
}

# ------------------------------------------------------------------------------
# Finds the curvature (change in angle per change in arc length)
.get_curvature <- function(x, y, stretch = TRUE)
{
  arclength <- .arclength_from_xy(x, y)
  angle     <- .path_angle_at_xy(x, y, degrees = FALSE)
  curvature <- diff(angle) / diff(arclength)
  if(stretch) .stretch_by_one(curvature) else curvature
}


# ------------------------------------------------------------------------------
# Finds the length of each part of the offset path that projects onto the
# original path
.length_adjust_by_curvature <- function(x, y, offset)
{
  curvature         <- .get_curvature(x, y) * 0.2
  length_correction <-  1 +  offset * curvature

  effective_length  <- c(0, diff(.arclength_from_xy(x, y))) * length_correction

  cumsum(effective_length)
}

