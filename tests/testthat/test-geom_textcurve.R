
test_that("The geom_textcurve() constructor works", {

  x <- geom_textcurve()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomTextpath")
  expect_s3_class(x$geom, "GeomTextcurve")
  expect_s3_class(x$stat, "StatIdentity")
})

test_that("The geom_labelcurve() constructor works", {

  x <- geom_labelcurve()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomLabelpath")
  expect_s3_class(x$geom, "GeomLabelcurve")
  expect_s3_class(x$stat, "StatIdentity")
})

test_that("geom_textcurve() creates grob appropriately", {

  p <- ggplot() +
    geom_textcurve(
      aes(x = c(0, 0), y = c(0, 1), xend = c(1, 1), yend = c(1, 0),
          label = c("ABC", "DEF"))
    )

  grobs <- layer_grob(p + coord_cartesian(expand = FALSE))[[1]]

  expect_s3_class(grobs, "textcurve")
  expect_s3_class(grobs$textpath, "textpath")
  expect_s3_class(grobs$curve, "curve")

  curve <- makeContent(grobs$curve)
  expect_s3_class(curve$children[[1]], "xspline")

  grobs <- makeContent(grobs)

  expect_s3_class(grobs, "textcurve")
  expect_s3_class(grobs$children[[1]], "textpath")
})

test_that("geom_labelcurve() creates grob appropriately", {

  p <- ggplot() +
    geom_labelcurve(
      aes(x = c(0, 0), y = c(0, 1), xend = c(1, 1), yend = c(1, 0),
          label = c("ABC", "DEF")),
      curvature = 0
    )

  grobs <- layer_grob(p + coord_cartesian(expand = FALSE))[[1]]

  expect_s3_class(grobs, "textcurve")
  expect_s3_class(grobs$textpath, "labelpath")
  expect_s3_class(grobs$curve, "curve")

  curve <- makeContent(grobs$curve)
  expect_s3_class(curve$children[[1]], "segments")

  grobs <- makeContent(grobs)

  expect_s3_class(grobs, "textcurve")
  expect_s3_class(grobs$children[[1]], "labelpath")
})
