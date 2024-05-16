
test_that("before and after give sensible outputs", {

  x <- numeric()

  expect_equal(length(before(x)), 0)
  expect_equal(length(after(x)), 0)

  x <- 1:5
  expect_equal(before(x), c(1, 1:5))
  expect_equal(after(x), c(1:5, 5))
})

test_that("angle_from_xy gives correct results", {

  # Equilateral triangle
  t <- seq(pi, -pi, length.out = 4)
  x <- cos(t)
  y <- sin(t)

  # Correct angles (*not* -30, 90, 30 as one might expect)
  ang <- angle_from_xy(x, y, degrees = TRUE)
  expect_equal(ang, c(30, -90, 150))

  # Reverse path should gave same angles as forward path, but negative
  ang_test <- angle_from_xy(rev(x), rev(y), degrees = TRUE)
  expect_equal(ang_test, -ang)

  # Radian and degree mode give same angles
  ang_test <- angle_from_xy(x, y, degrees = FALSE)
  expect_equal(ang_test, ang / 180 * pi)

  # Normal should be 90 degrees offset
  ang_test <- angle_from_xy(x, y, norm = TRUE, degrees = TRUE)
  expect_equal(ang_test, ang + 90)

  # Should be able to handle length-2 vectors
  ang_test <- angle_from_xy(x[1:2], y[1:2], degrees = TRUE)
  expect_equal(ang_test, 30)

  x <- unit(c(0, 1), "in")
  y <- unit(c(0, 1), "in")
  expect_equal(angle_from_xy(x, y, degrees = TRUE), 45)
})

test_that("arclength_from_xy gives correct results", {

  # Vector mode
  lens   <- runif(50)
  angles <- runif(50, max = 2 * pi)

  x <- cumsum(lens * cos(angles))
  y <- cumsum(lens * sin(angles))

  arclen <- arclength_from_xy(x, y)
  expect_equal(arclen[50], sum(lens[2:50]))

  # Matrix mode
  x <- cbind(x[1:25], x[26:50])
  y <- cbind(y[1:25], y[26:50])

  arclen <- arclength_from_xy(x, y)
  expect_equal(arclen[25, ], c(sum(lens[2:25]), sum(lens[27:50])))

  x <- unit(c(0, 3), "npc")
  y <- unit(c(0, 4), "npc")

  expect_equal(arclength_from_xy(x, y), c(0, 5))

  expect_equal(arclength_from_xy(0, 0), 0)
  expect_equal(arclength_from_xy(0, NA), as.numeric(NA))
  expect_equal(arclength_from_xy(NA, 0), as.numeric(NA))

})

test_that("get_offset offsets correctly", {

  x <- c(1:5)
  y <- c(1, 2, 3, 2, 1)

  offset <- get_offset(x, y, d = 1:2 / sqrt(2))

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

  curv_1 <- get_curvature(x, y)

  # the curvature should be the reciprocal of the radius
  radius_1 <- 1 / curv_1

  expect_true(
    all(abs(radius_1 - 1) < 0.001)
  )

  # Doubling the radius of the circle should half the curvature
  curv_2 <- get_curvature(2 * x, 2 * y)

  expect_true(
    all(abs((curv_1 / curv_2) - 2) < 0.001)
  )
})

test_that("We can roll our own means", {

  val <- c(4.25, 5.75, 5.75, 6, 5.5, 5.5, 5, 6, 5, 3.5, 4)
  expect_equal(val, safe_rollmean(c(3, 8, 9, 3, 4, 6, 9, 1, 8, 2, 3), k = 4))
  expect_equal(val, safe_rollmean(val, k = 1))
})


test_that("We can find the flattest point of a curve", {

  x <- 1:100
  y <- sin(seq(0, 2 * pi, len = 100))
  expect_equal(which.min_curvature(x, y), 1)
})


test_that("We can smooth a noisy path", {
  x <- seq(0, 2 * pi, len = 100)
  y <- sin(x) + 0.3 * sin(x * 20)
  id <- rep(1, 100)
  length <- arclength_from_xy(x, y)
  df <- data.frame(x = x, y = y, length = length, id = id)

  df$x <- grid::unit(x, "npc")
  df$y <- grid::unit(y, "npc")

  a <- smooth_noisy(df, 1)

  x1 <- as.numeric(a$x)
  y1 <- as.numeric(a$y)

  expect_true(has_corners(x, y))
  expect_false(has_corners(x1, y1))
})

test_that("We can get a 1-d quadratic Bezier", {

  expected <- (0:10 - 5)^2
  actual   <- quad_bezier(25, -25, 25, seq(0, 1, 0.1))
  expect_equal(actual, expected)
})

test_that("Corners are smoothed appropriately", {

  expected <- structure(list(x = c(0, 1, 4, 9, 16),
                             y = c(0, 7, 12, 15, 16),
                             line_x = c(0, 0, 0, 8, 16),
                             line_y = c(0, 8, 16, 16, 16),
                             line_length = c(0, 8, 16, 24, 32)),
                        class = "data.frame", row.names = c(NA, -5L))
  actual   <- corner_smoother(c(0, 0), c(0, 1), c(1, 1), p = 5)
  expect_equal(actual[-3], expected / 16)

  p1 <- p2 <- p3 <- c(0, 1)
  nms <- c("x", "y", "length", "line_x", "line_y", "line_length")

  expect <- matrix(rep(c(0, 1, 0, 0, 1, 0), each = 20), nrow = 20)
  expect <- setNames(as.data.frame(expect), nms)
  expect_equal(corner_smoother(p1, p2, p3), expect)

  df <- data.frame(x = rep(0, 11), y = 0:10, length = 0:10, line_x = 0,
                   line_y = 0:10, line_length = 0:10)

  expect_equal(corner_smoother(c(0, 0), c(0, 10), c(0, 10), 11), df)
  expect_equal(corner_smoother(c(0, 0), c(0, 0), c(0, 10), 11), df)
})

test_that("We can make a data frame of points between two given points", {

  actual <- linear_smooth(p1 = c(0, 0), p2 = c(0, 10), n = 11)

  df <- data.frame(x = rep(0, 11), y = 0:10, length = 0:10, line_x = 0,
                   line_y = 0:10, line_length = 0:10)

  expect_equal(actual, df)
})

test_that("We can get Bezier control points from segments and paths", {

  x <- 0
  y <- 0
  ang <- pi / 4
  len <- sqrt(2)
  radius <- 0.1
  d <-  sqrt(0.02) / 2

  expect <- matrix(rep(c(0, d, 0.5, 1 - d), 2), ncol = 2)

  expect_equal(segment_control_points(x, y, len, ang, radius), expect)

  expect_equal(segment_control_points(x, y, len, ang, 1),
               matrix(c(0, 1, 0, 1), 2) / 2)

  actual <- find_control_points(data.frame(x = 0:2, y = c(0, 1, 0)), 0.1)

  expect <- c(c(0, 0, d, 0.5, 1 - d, 1, 1 + d, 1.5, 2 - d, 2, 2),
              c(0, 0, d, 0.5, 1 - d, 1, 1 - d, 0.5, d, 0, 0))
  expect <- matrix(expect, ncol = 2)

  expect_equal(actual, expect)

  sc <- smooth_corners(data.frame(x = 0:2, y = c(0, 1, 0)), n = 3, radius = 0.1)

  vec <- c(0, d / 2, d, d, 0.5, 1 - d, 1 - d)
  x <-  c(vec, 1, 2 - rev(vec))
  y <- c(vec, 1 - d / 2, rev(vec))
  len <- cumsum(c(0, sqrt(diff(x)^2 + diff(y)^2)))
  line_len <- c(0, 0.05, 0.1, 0.1, sqrt(2) / 2, sqrt(2) - 0.1, sqrt(2) - 0.1,
                sqrt(2), sqrt(2) + 0.1, sqrt(2) + 0.1, sqrt(2) * 3 / 2,
                2 * sqrt(2) - 0.1, 2 * sqrt(2) - 0.1,
                2*sqrt(2) - 0.05, 2 * sqrt(2))
  df <- data.frame(x = x, y = y, length = len,
                   line_x = x, line_y = y, line_length = line_len,
                   segment = rep(1:5, each = 3))
  df$line_y[8] <- 1

  expect_equal(sc, df)
})

test_that("We can apply both smoothing types", {

  x <- unit((0:2) / 2, "npc")
  y <- unit(c(0, 1, 0), "npc")
  label <- "X"
  id <- c(1, 1, 1)

  png("Rplot_test.png", width = 7, height = 7, units = "in", res = 100)
  grob <- textpathGrob(label, x, y, id, text_smoothing = 50)
  grob <- makeContent(grob)
  x <- convertUnit(grob$children[[2]]$x, "npc", valueOnly = TRUE)
  y <- convertUnit(grob$children[[2]]$y, "npc", valueOnly = TRUE)
  dev.off()
  unlink("Rplot_test.png")

  expect_lt(abs(x - 0.5), 0.003)
  expect_lt(abs(y - 0.9852688), 0.003)
})
