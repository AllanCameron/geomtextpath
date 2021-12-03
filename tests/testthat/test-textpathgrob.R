
test_that("textpathGrobs can be created", {

  # Default empty textpathGrob
  expect_silent(textpathGrob())

  # Single point-like
  expect_silent(textpathGrob(label = "Hello"))

  # Single path-like
  expect_silent(textpathGrob(label = "Hello",
                             x = 0:1, y = 0:1, id = c(1, 1)))

  # Multiple point-like
  expect_silent(textpathGrob(label = c("Hello", "World"),
                             x = c(1, 2),
                             y = c(1, 2),
                             id = c(1, 2)))

  # Multiple path-like
  expect_silent(textpathGrob(label = c("Hello", "World"),
                             x = c(0, 1, 1.5, 2),
                             y = c(0, 1, 2, 1),
                             id = c(1, 1, 2, 2)))

  # Mixed points and paths
  expect_silent(textpathGrob(label = c("Hello", "World", "lorem", "ipsum"),
                             x = c(0, 1, 1.5, 2, 3, 4),
                             y = c(0, 1, 2, 1, 3, 4),
                             id = c(1, 1, 2, 2, 3, 4)))

  # Mixed points and paths with angles and polar parameters
  expect_silent(textpathGrob(label = c("Hello", "World", "lorem", "ipsum"),
                             x = c(0, 1, 1.5, 2, 3, 4),
                             y = c(0, 1, 2, 1, 3, 4),
                             id = c(1, 1, 2, 2, 3, 4),
                             angle = 0,
                             polar_params = list(x = .5, y = .5, theta = "x")))

  # Plotmath expression with point-like path
  expect_silent(textpathGrob(label = expression(paste("y = ", x^2))))

  # Plotmath expressions with paths
  expect_silent(textpathGrob(label = c(expression(paste("y = ", x^2)),
                                      expression(paste("x = ", y^2))),
                              x = c(0, 1, 0, 1),
                              y = c(0, 1, 0, 0.5),
                              id = c(1, 1, 2, 2)))

  # Error should be thrown with invalid input
  expect_error(textpathGrob(label = c("Hello", "World", "lorem", "ipsum"),
                             x = c(0, 1, 1.5, 2, 3, 4),
                             y = c(0, 1, 2, 1, 3, 4),
                             id = c(1, 1, 2, 3, 4),
                             angle = 0,
                             polar_params = list(x = .5, y = .5, theta = "x")),
               "not of the same length")

})

test_that("We can correctly pathify points", {

  data   <- data.frame(x = 0.75, y = 0.2, id = 1)

  # linear pathify
  linear <- .pathify(data, hjust = 0.5, angle = 45, width = 1,
                     polar_x = NA, polar_y = NA, thet = NA)
  # Polar pathify
  polar  <- .pathify(data, hjust = 0.5, angle = 45, width = 1,
                     polar_x = 0.5, polar_y = 0.5, thet = "y")

  expect_equal(nrow(linear), 100L)
  expect_equal(nrow(polar), 100L)

  expect_true(abs(polar$x[1] - 0.2290784) < 1e-6)
  expect_true(abs(linear$x[1] - 0.3964466) < 1e-6)

})
