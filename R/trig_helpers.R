# Constants ---------------------------------------------------------------

.rad2deg <- 180 / pi
.deg2rad <- pi / 180
.halfpi  <- pi /2

# ------------------------------------------------------------------------------
# We can only define paths if they have two or more valid numeric points, and no
# path can contain any infinite values

.check_xy <- function(x, y) {

  stopifnot("x and y must be the same length" = {(n <- length(x)) == length(y)},
            "x and y must both be numeric" = {(is.numeric(x) & is.numeric(y))},
            "x and y must be length 2 or more" = {n > 1},
            "x and y must not contain infinite points" =
              {all(is.finite(x[!is.na(x)]) & is.finite(y[!is.na(y)]))})
}

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
  .check_xy(x, y)
  x <- .interp_na(x)
  y <- .interp_na(y)

  grad       <- diff(y) / diff(x)
  first     <- atan2(diff(y[1:2]), diff(x[1:2]))
  diff_rads <- diff(atan(grad))
  diff_rads[i] <- diff_rads[{i <- diff_rads < - .halfpi}] + pi
  diff_rads[i] <- diff_rads[{i <- diff_rads > + .halfpi}] - pi
  rads      <- cumsum(c(first, diff_rads))
  if(norm) rads <- rads + .halfpi
  if(degrees) rads * .rad2deg else rads
}

# ------------------------------------------------------------------------------
# Get the cumulative length of an x, y path. The accuracy can be improved by
# setting accuracy to 1 or more, which will interpolate the points with splines
# to emulate a smooth curve through the points.

.arclength_from_xy <- function(x, y, accuracy = NA)
{
  .check_xy(x, y)

  if (is.matrix(x) || is.matrix(y)) {
    stopifnot(
      "Both or neither x and y must be matrices" =
        is.matrix(x) && is.matrix(y)
    )
      # Call self for every column, even if accuracy is NA, so that any
      # NA values are handled appropriately

      out <- Map(.arclength_from_xy, x = asplit(x, 2), y = asplit(y, 2),
                 accuracy = accuracy)
      out <- do.call(cbind, out)

    return(out)
  }

  x <- .interp_na(x)
  y <- .interp_na(y)

  if(is.na(accuracy)) return(c(0, cumsum(sqrt(diff(x)^2 + diff(y)^2))))

  stopifnot(
    "accuracy must be a positive integer" =
      is.numeric(accuracy) & length(accuracy) == 1 & accuracy >= 0
  )

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

.before <- function(x) {

  if(length(x) == 0) x else x[c(1, seq_along(x))]
}

# ------------------------------------------------------------------------------

.after <- function(x) {

  if(length(x) == 0) x else x[c(seq_along(x), length(x))]
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

.average_segments_at_points <- function(x) {

  (.before(x) + .after(x)) / 2
}

# ------------------------------------------------------------------------------
# Finds the offset path at distance d. This method effectively looks at each
# segment of the path and finds the line at distance d that runs parallel to
# it. The offset path is the set of points where adjacent offset lines meet.

.get_offset <- function(x, y, d = 0) {

  # Get angle normal to each segment of the path
  theta <- .angle_from_xy(x, y, norm = TRUE)

  # Find the angle of the lines which, when drawn at each point on the path
  # will project onto the intersections between adjacent offset segments
  theta_bisect <- .average_segments_at_points(theta)

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
  arc_length <- .arclength_from_xy(xout, yout)

  return(list(x = xout, y = yout, arc_length = arc_length))
}

# ------------------------------------------------------------------------------
# Finds the curvature (change in angle per change in arc length)
# This in effect finds 1/R, where R is the radius of the curve

.get_curvature <- function(x, y)
{
  if(length(x) < 3) return(rep(0, length(x)))

  dx  <- diff(x)
  ddx <- diff(dx)
  dy  <- diff(y)
  ddy <- diff(dy)
  dx  <- head(dx, -1)
  dy  <- head(dy, -1)

  curv <- (dx * ddy - ddx * dy) / (dx^2 + dy^2)^(3/2)

  # Duplicate first and last entries, since these are the best estimates
  # of the curvature at these points, which is otherwise undefined.
  .before(.after(curv))
}

.exceeds_curvature <- function(x, y, d, tolerance = 0.1)
{
  curve_radius <- 1 / .get_curvature(x, y)
  as.numeric(apply(outer(curve_radius, d,
        FUN = function(a, b) {
          (abs(a) < abs(b)) & (sign(a) == sign(b))
        }), 1, any))
}


