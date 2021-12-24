
test_that("The geom_textpath() constructor works", {

  x <- geom_textpath(stat = "density")

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomTextpath")
  expect_s3_class(x$stat, "StatDensity")
})

test_that("The geom_textline() constructor works", {

  x <- geom_textline()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomTextLine")
  expect_s3_class(x$stat, "StatIdentity")
})

test_that("The geom_textdensity() constructor works", {

  x <- geom_textdensity()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomTextDensity")
  expect_s3_class(x$stat, "StatDensity")
})

test_that("The geom_textsmooth() constructor works", {

  x <- geom_textsmooth()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomTextpath")
  expect_s3_class(x$stat, "StatSmooth")
})

test_that("The geom_labelline() constructor works", {

  x <- geom_labelline()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomLabelpath")
  expect_s3_class(x$stat, "StatIdentity")
})

test_that("The geom_labeldensity() constructor works", {

  x <- geom_labeldensity()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomLabelpath")
  expect_s3_class(x$stat, "StatDensity")
})

test_that("The geom_labelsmooth() constructor works", {

  x <- geom_labelsmooth()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomLabelpath")
  expect_s3_class(x$stat, "StatSmooth")
})

test_that("The geom_labelcontour() constructor works", {

  x <- geom_labelcontour()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomLabelpath")
  expect_s3_class(x$stat, "StatTextContour")
})

test_that("The geom_labeldensity2d() constructor works", {

  x <- geom_labeldensity2d()

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomLabelDensity2d")
  expect_s3_class(x$stat, "StatDensity2d")
})

test_that("Text path grob has correct types", {

  p <- ggplot(iris, aes(Sepal.Width)) +
    geom_textpath(aes(label = Species, group = Species), stat = "density")

  grobs <- layer_grob(p)[[1]]

  expect_s3_class(grobs, "gTree")
  expect_s3_class(grobs, "textpath")
  expect_length(grobs$children, 0)

  grobs <- makeContent(grobs)

  expect_length(grobs$children, 2)
  expect_s3_class(grobs$children[[1]], "polyline")
  expect_s3_class(grobs$children[[2]], "text")
})
