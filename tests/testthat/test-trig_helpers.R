
test_that(".before and .after give sensible outputs", {

  x <- numeric()

  expect_equal(length(.before(x)), 0)
  expect_equal(length(.after(x)), 0)

  x <- 1:5
  expect_equal(.before(x), c(1, 1:5))
  expect_equal(.after(x), c(1:5, 5))
})

test_that(".angle_from_xy gives correct results", {

  # Equilateral triangle
  t <- seq(pi, -pi, length.out = 4)
  x <- cos(t)
  y <- sin(t)

  # Correct angles (*not* -30, 90, 30 as one might expect)
  ang <- .angle_from_xy(x, y, degrees = TRUE)
  expect_equal(ang, c(30, 90, 150))

  # Reverse path should gave same angles as forward path, but negative
  ang_test <- .angle_from_xy(rev(x), rev(y), degrees = TRUE)
  expect_equal(ang_test, -ang)

  # Radian and degree mode give same angles
  ang_test <- .angle_from_xy(x, y, degrees = FALSE)
  expect_equal(ang_test, ang / 180 * pi)

  # Normal should be 90 degrees offset
  ang_test <- .angle_from_xy(x, y, norm = TRUE, degrees = TRUE)
  expect_equal(ang_test, ang + 90)

  # Should be able to handle length-2 vectors
  ang_test <- .angle_from_xy(x[1:2], y[1:2], degrees = TRUE)
  expect_equal(ang_test, 30)
})

test_that(".arclength_from_xy gives correct results", {

  # Vector mode
  lens   <- runif(50)
  angles <- runif(50, max = 2 * pi)

  x <- cumsum(lens * cos(angles))
  y <- cumsum(lens * sin(angles))

  arclen <- .arclength_from_xy(x, y)
  expect_equal(arclen[50], sum(lens[2:50]))

  # Matrix mode
  x <- cbind(x[1:25], x[26:50])
  y <- cbind(y[1:25], y[26:50])

  arclen <- .arclength_from_xy(x, y)
  expect_equal(arclen[25,], c(sum(lens[2:25]), sum(lens[27:50])))

  # Ensure accuracy is improved with accuracy parameter

  t <- seq(0, 360, length.out = 1000) * pi / 180
  x <- cos(t)
  y <- sin(t)

  basic <- abs(max(.arclength_from_xy(x, y)) - 2 * pi)
  accurate <-  abs(max(.arclength_spline(x, y, accuracy = 5)) - 2 * pi)

  expect_lt(accurate, basic)

  accurate <- abs(max(.arclength_spline(x, y, accuracy = NA)) - 2 * pi)
  expect_equal(accurate, basic)
})

test_that(".get_offset offsets correctly", {

  x <- c(1:5)
  y <- c(1,2,3,2,1)

  offset <- .get_offset(x, y, d = 1:2 / sqrt(2))

  expect_equal(
    offset$x,
    cbind(c(0.5, 1.5, 3, 4.5, 5.5), c(0, 1, 3, 5, 6))
  )

  expect_equal(
    offset$y,
    cbind(c(1.5, 2.5, 4, 2.5, 1.5), c(2, 3, 5, 3, 2))
  )

  expect_equal(
    offset$arc_length,
    cbind(c(0, 2, 5, 8, 10), c(0, 2, 6, 10, 12)) / sqrt(2)
  )
})

test_that("We can measure curvature accurately", {

  # x and y describe a circle with radius 1:
  t <- seq(0, 360, length.out = 1000) * pi / 180
  x <- cos(t)
  y <- sin(t)

  curv_1 <- .get_curvature(x, y)

  # the curvature should be the reciprocal of the radius
  radius_1 <- 1 / curv_1

  expect_true(
    all(abs(radius_1 - 1) < 0.001)
  )

  # Doubling the radius of the circle should half the curvature
  curv_2 <- .get_curvature(2 * x, 2 * y)

  expect_true(
    all( abs((curv_1 / curv_2) - 2) < 0.001)
  )
})

test_that("We can roll our own means", {

  val <- c(4.25, 5.75, 5.75, 6, 5.5, 5.5, 5, 6, 5, 3.5, 4)
  expect_equal(val, .safe_rollmean(c(3, 8, 9, 3, 4, 6, 9, 1, 8, 2, 3), k = 4))
  expect_equal(val, .safe_rollmean(val, k = 1))
})


test_that("We can find the flattest point of a curve", {

  x <- 1:100
  y <- sin(seq(0, 2 * pi, len = 100))
  expect_equal(which.min_curvature(x, y), 1)
})

test_that("We can identify a path with 'corners'", {
  x <- 1:10
  y <- c(1, 2.1, 3.3, 4.6, 6.0, 7.5, 9.1, 10.8, 12.6, 14.5)
  z <- c(0, 1, 1, 0, 6.0, 7.5, 9.1, 10.8, 12.6, 14.5)
  expect_false(has_corners(x, y))
  expect_true(has_corners(x, z))
})


test_that("We can spline smooth", {

  x <- c(1, 4, 5, 6)
  y <- c(1, 1.9, 2.6, 3.2, 3.6, 4, 4.3, 4.5,
         4.7, 4.9, 5, 5.1, 5.3, 5.5, 5.7, 6)
  z <- round(spline_smooth(x), 1)
  expect_equal(y, z)
})

test_that("We can chunk a path", {

  x <- seq(0, 2 * pi, len = 100)
  y <- sin(x) + 0.3 * sin(x * 20)
  z <- sample_path(x, y, n = 10)
  expect_equal(sum(z), 28.336833)
})

test_that("We can smooth a noisy path", {
  x <- seq(0, 2 * pi, len = 100)
  y <- sin(x) + 0.3 * sin(x * 20)
  a <- smooth_noisy(x, y, 20)
  expect_true(has_corners(x, y))
  expect_false(has_corners(a[,1], a[,2]))
})

test_that("We can get a 1-d quadratic Bezier", {

  expected <- (0:10 - 5)^2
  actual   <- quad_bezier(25, -25, 25, seq(0, 1, 0.1))
  expect_equal(actual, expected)
})

test_that("Corners are smoothed appropriately", {

  expected <- cbind((0:4)^2, (16 - (4:0)^2)) / 16
  actual   <- corner_smoother(0, 0, 0, 1, 1, 1, p = 5)
  expect_equal(actual, expected)
})

test_that("We get correct control points for path smoothing", {

  x <- 1:5
  y <- c(0, 1, 1, 0.5, 1)
  expected <- structure(c(1, 1, 1.35355339059327, 1.5, 1.64644660940673, 2,
                          2.5, 3, 3.44721359549996, 3.5, 3.55278640450004, 4,
                          4.44721359549996, 4.5, 4.55278640450004, 5, 5, 0, 0,
                          0.353553390593274, 0.5, 0.646446609406726,
                          1, 1, 1, 0.776393202250021, 0.75, 0.723606797749979,
                          0.5, 0.723606797749979, 0.75, 0.776393202250021,
                          1, 1), .Dim = c(17L, 2L))
  actual <- find_control_points(x, y)
  expect_equal(actual, expected)
})

test_that("We can Bezier smooth a path", {

  x <- 1:5
  y <- c(0, 1, 1, 0.5, 1)
  expected <- structure(
    c(1, 1.08838834764832, 1.35355339059327, 1.35355339059327,
      1.5, 1.64644660940673, 1.64644660940673, 2.03661165235168, 2.5,
      2.5, 2.98680339887499, 3.44721359549996, 3.44721359549996, 3.5,
      3.55278640450004, 3.55278640450004, 4, 4.44721359549996, 4.44721359549996,
      4.5, 4.55278640450004, 4.55278640450004, 4.88819660112501, 5,
      0, 0.0883883476483184, 0.353553390593274, 0.353553390593274,
      0.5, 0.646446609406726, 0.646446609406726, 0.911611652351682,
      1, 1, 0.944098300562505, 0.776393202250021, 0.776393202250021,
      0.75, 0.723606797749979, 0.723606797749979, 0.611803398874989,
      0.723606797749979, 0.723606797749979, 0.75, 0.776393202250021,
      0.776393202250021, 0.944098300562505, 1), .Dim = c(24L, 2L))
  actual <- smooth_corners(x, y, n = 3, radius = 0.5)

  expect_equal(actual, expected)
})
