

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
    all(curv_1 / curv_2 == 2)
  )
})
