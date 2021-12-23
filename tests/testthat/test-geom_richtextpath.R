
test_that("The geom_richtextpath() constructor works", {

  skip_if_not_installed("xml2")
  skip_if_not_installed("markdown")

  x <- geom_richtextpath(stat = "density")

  expect_s3_class(x, "LayerInstance")
  expect_s3_class(x$geom, "GeomTextpath")
  expect_s3_class(x$geom, "GeomRichtextpath")
  expect_s3_class(x$stat, "StatDensity")
})

test_that("Text path grob has correct types", {

  df <- data_frame(
    x = c(1, 2, 1, 2),
    y = c(1, 2, 2, 1),
    lab = rep(c("<span style='color:red'>red</span> text",
                "<sub>sub</sub>text is<sup>super</sup>"), each = 2)
  )

  p <- ggplot(df, aes(x, y, label = lab)) +
    geom_richtextpath(hjust = 0.25)

  grobs <- layer_grob(p)[[1]]

  expect_s3_class(grobs, "gTree")
  expect_s3_class(grobs, "textpath")
  expect_length(grobs$children, 0)

  expect_equal(
    grobs$textpath$label[[c(1, 1)]][-4],
    unlist(strsplit("red text", ""))[-4]
  )

  expect_equal(
    grobs$textpath$label[[c(2, 1)]][-8],
    unlist(strsplit("subtext issuper", ""))[-8]
  )

  grobs <- makeContent(grobs)

  expect_length(grobs$children, 2)
  expect_s3_class(grobs$children[[1]], "polyline")
  expect_s3_class(grobs$children[[2]], "text")

  expect_equal(
    grobs$children[[2]]$gp$col,
    rep(c("red", "#000000"), times = c(3, 20))
  )
})
