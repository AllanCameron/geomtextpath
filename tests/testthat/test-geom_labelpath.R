test_that("The geom_labelpath() constructor works", {

  x <- geom_labelpath(stat = "density")

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomTextpath")
  expect_s3_class(x$geom, "GeomLabelpath")
  expect_s3_class(x$stat, "StatDensity")
})

test_that("Text path grob has correct types", {

  p <- ggplot(iris, aes(Sepal.Width)) +
    geom_labelpath(aes(label = Species, group = Species), stat = "density")

  grobs <- layer_grob(p)[[1]]

  expect_s3_class(grobs, "gTree")
  expect_s3_class(grobs, "textpath")
  expect_length(grobs$children, 0)

  grobs <- makeContent(grobs)

  expect_length(grobs$children, 3)
  expect_s3_class(grobs$children[[1]], "polyline")
  expect_s3_class(grobs$children[[2]], "polygon")
  expect_s3_class(grobs$children[[3]], "text")
})
