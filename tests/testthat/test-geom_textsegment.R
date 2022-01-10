
test_that("The geom_textsegment() constructor works", {

  x <- geom_textsegment()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomTextpath")
  expect_s3_class(x$geom, "GeomTextsegment")
  expect_s3_class(x$stat, "StatIdentity")
})

test_that("The geom_labelsegment() constructor works", {

  x <- geom_labelsegment()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomLabelpath")
  expect_s3_class(x$geom, "GeomLabelsegment")
  expect_s3_class(x$stat, "StatIdentity")
})

test_that("geom_textsegment() creates grob appropriately", {

  p <- ggplot() +
    geom_textsegment(
      aes(x = c(0, 0), y = c(0, 1), xend = c(1, 1), yend = c(1, 0),
          label = c("ABC", "DEF"))
    )

  grobs <- layer_grob(p + coord_cartesian(expand = FALSE))[[1]]
  data  <- grobs$textpath$data

  expect_equal(convertUnit(data$x, "npc", valueOnly = TRUE), c(0, 1, 0, 1))
  expect_equal(convertUnit(data$y, "npc", valueOnly = TRUE), c(0, 1, 1, 0))

  label <- grobs$textpath$label
  # Should be kept straight (i.e. 1 row) because its not polar
  expect_length(label[[1]]$glyph, 1)
  expect_length(label[[2]]$glyph, 1)

  # Should be length 3 if coord was polar, because letters should be split
  grobs <- layer_grob(p + coord_polar())[[1]]
  label <- grobs$textpath$label

  expect_length(label[[1]]$glyph, 3)
  expect_length(label[[2]]$glyph, 3)
})

test_that("geom_labelsegment() creates grob appropriately", {

  p <- ggplot() +
    geom_labelsegment(
      aes(x = c(0, 0), y = c(0, 1), xend = c(1, 1), yend = c(1, 0),
          label = c("ABC", "DEF"))
    )

  grobs <- layer_grob(p + coord_cartesian(expand = FALSE))[[1]]
  data  <- grobs$textpath$data

  expect_equal(convertUnit(data$x, "npc", valueOnly = TRUE), c(0, 1, 0, 1))
  expect_equal(convertUnit(data$y, "npc", valueOnly = TRUE), c(0, 1, 1, 0))

  label <- grobs$textpath$label
  # Should be kept straight (i.e. 1 row) because its not polar
  expect_length(label[[1]]$glyph, 1)
  expect_length(label[[2]]$glyph, 1)
})

test_that("segment2path gives zeroGrob in right circumstances", {
  # This is mostly to cover that one line of code
  test <- segment2path(NULL)
  expect_s3_class(test, "zeroGrob")

  test <- segment2path(data.frame(x = integer()))
  expect_s3_class(test, "zeroGrob")

  test <- segment2path(data.frame(row.names = 1:10))
  expect_s3_class(test, "zeroGrob")
})


test_that("The geom_texthline constructor works as expected", {
  x <- geom_texthline()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomTextpath")
  expect_s3_class(x$geom, "GeomTexthline")
  expect_s3_class(x$stat, "StatIdentity")

  p <- ggplot(mtcars, aes(mpg, disp)) + geom_point()
  expect_silent(p + geom_texthline(yintercept = 200, label = "test"))

  expect_warning(p + geom_texthline(aes(yintercept = disp),
                                    label = "test", yintercept = 200))

  w <- capture_warnings(p + geom_texthline(aes(yintercept = disp),
                                           label = "test",
                                           yintercept = 200,
                                           data = mtcars))
  expect_true(grepl("mapping", w[1]))
  expect_true(grepl("data", w[2]))
})

test_that("The geom_labelhline constructor works as expected", {
  x <- geom_labelhline()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomLabelpath")
  expect_s3_class(x$geom, "GeomLabelhline")
  expect_s3_class(x$stat, "StatIdentity")

  p <- ggplot(mtcars, aes(mpg, disp)) + geom_point()
  expect_silent(p + geom_labelhline(yintercept = 200, label = "test"))

  expect_warning(p + geom_labelhline(aes(yintercept = disp),
                                    label = "test", yintercept = 200))

  w <- capture_warnings(p + geom_labelhline(aes(yintercept = disp),
                                           label = "test",
                                           yintercept = 200,
                                           data = mtcars))
  expect_true(grepl("mapping", w[1]))
  expect_true(grepl("data", w[2]))
})

test_that("The geom_textvline constructor works as expected", {
  x <- geom_textvline()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomTextpath")
  expect_s3_class(x$geom, "GeomTextvline")
  expect_s3_class(x$stat, "StatIdentity")

  p <- ggplot(mtcars, aes(mpg, disp)) + geom_point()
  expect_silent(p + geom_textvline(xintercept = 20, label = "test"))

  expect_warning(p + geom_textvline(aes(xintercept = mpg),
                                    label = "test", xintercept = 20))

  w <- capture_warnings(p + geom_textvline(aes(xintercept = mpg),
                                           label = "test",
                                           xintercept = 20,
                                           data = mtcars))
  expect_true(grepl("mapping", w[1]))
  expect_true(grepl("data", w[2]))
})

test_that("The geom_labelvline constructor works as expected", {
  x <- geom_labelvline()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomLabelpath")
  expect_s3_class(x$geom, "GeomLabelvline")
  expect_s3_class(x$stat, "StatIdentity")

  p <- ggplot(mtcars, aes(mpg, disp)) + geom_point()
  expect_silent(p + geom_labelvline(xintercept = 20, label = "test"))

  expect_warning(p + geom_labelvline(aes(xintercept = mpg),
                                    label = "test", xintercept = 20))

  w <- capture_warnings(p + geom_labelvline(aes(xintercept = mpg),
                                           label = "test",
                                           xintercept = 20,
                                           data = mtcars))
  expect_true(grepl("mapping", w[1]))
  expect_true(grepl("data", w[2]))
})

test_that("The geom_textabline constructor works as expected", {
  x <- geom_textabline()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomTextpath")
  expect_s3_class(x$geom, "GeomTextabline")
  expect_s3_class(x$stat, "StatIdentity")

  p <- ggplot(mtcars, aes(mpg, disp)) + geom_point()
  expect_silent(p + geom_textabline(intercept = -100, slope = 15,
                                    label = "test"))

  expect_warning(p + geom_textabline(aes(intercept = disp, slope = mpg),
                                    label = "test", intercept = -100,
                                    slope = 15))

  w <- capture_warnings(p + geom_textabline(aes(intercept = disp, slope = mpg),
                                           label = "test", intercept = -100,
                                           slope = 15,
                                           data = mtcars))
  expect_true(grepl("mapping", w[1]))
  expect_true(grepl("data", w[2]))

  expect_equal(unclass(geom_textabline(slope = 1)$mapping)$intercept, 0)
  expect_equal(unclass(geom_textabline(intercept = 1)$mapping)$slope, 1)
})

test_that("The geom_labelabline constructor works as expected", {
  x <- geom_labelabline()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomLabelpath")
  expect_s3_class(x$geom, "GeomLabelabline")
  expect_s3_class(x$stat, "StatIdentity")

  p <- ggplot(mtcars, aes(mpg, disp)) + geom_point()
  expect_silent(p + geom_labelabline(intercept = -100, slope = 15,
                                    label = "test"))

  expect_warning(p + geom_labelabline(aes(intercept = disp, slope = mpg),
                                    label = "test", intercept = -100,
                                    slope = 15))

  w <- capture_warnings(p + geom_labelabline(aes(intercept = disp, slope = mpg),
                                           label = "test", intercept = -100,
                                           slope = 15,
                                           data = mtcars))
  expect_true(grepl("mapping", w[1]))
  expect_true(grepl("data", w[2]))

  expect_equal(unclass(geom_labelabline(slope = 1)$mapping)$intercept, 0)
  expect_equal(unclass(geom_labelabline(intercept = 1)$mapping)$slope, 1)
})
