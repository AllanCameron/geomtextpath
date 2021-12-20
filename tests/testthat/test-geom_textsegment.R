
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
