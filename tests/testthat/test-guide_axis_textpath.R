test_that("guide_axis_textpath constructor works", {

  # Test classes
  x <- guide_axis_textpath()

  expect_s3_class(x, "Guide")
  expect_s3_class(x, "GuideAxis")
  expect_s3_class(x, "GuideAxisTheta")

  # Test args are passed correctly
  x <- guide_axis_textpath(cap = TRUE)
  expect_equal(x$params$cap, "both")
  x <- guide_axis_textpath(cap = FALSE)
  expect_equal(x$params$cap, "none")

  expect_error(guide_axis_textpath(cap = NA))
  expect_error(guide_axis_textpath(minor.ticks = NA))

})

test_that("guide_axis_textpath can render a grob", {

  p <- ggplot(iris, aes(Species, Sepal.Width)) +
    geom_boxplot() +
    coord_radial() +
    guides(theta = "axis_textpath")

  gt <- ggplotGrob(p)

  # Excavate the guide text from the panel grob
  grob <- gt$grobs[gt$layout$name == "panel"][[1]]
  grob <- grob$children[[length(grob$children)]]
  grob <- grob$children[[1]]
  grob <- grob$children[[2]]

  expect_s3_class(grob, "textpath")
})
