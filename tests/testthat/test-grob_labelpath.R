test_that("label can be missing", {

  case <- textpathGrob(x = c(0, 1), y = c(0, 1), id = c(1, 1), as_label = TRUE)
  ctrl <- textpathGrob(x = c(0, 1), y = c(0, 1), id = c(1, 1), label = "ABC",
                        as_label = TRUE)

  expect_null(case$textpath)
  expect_type(ctrl$textpath, "list")

  case <- makeContent(case)
  ctrl <- makeContent(ctrl)

  expect_s3_class(case, "zeroGrob")
  expect_s3_class(ctrl, "gTree")

  # Test that polar parameters are converted
  test <- textpathGrob(
    x = c(0, 1), y = c(0, 1), id = c(1, 1), label = "ABC",
    polar_params = list(x = 0.5, y = 0.5), as_label = TRUE
  )
  ppar <- test$textpath$params$polar_params
  expect_equal(convertUnit(ppar$x, "npc", valueOnly = TRUE), 0.5)
  expect_equal(convertUnit(ppar$y, "npc", valueOnly = TRUE), 0.5)

})


test_that("straight and curved setting produce similar boxes", {

  pth <- textpathGrob(
    "ABC",
    x = c(0, 1),
    y = c(0, 1),
    id = c(1, 1),
    gp_box = gpar(fill = "white"),
    as_label = TRUE
  )
  pth <- makeContent(pth)

  box1 <- pth$children[[2]]

  pth <- textpathGrob(
    "ABC",
    x = c(0, 1),
    y = c(0, 1),
    id = c(1, 1),
    gp_box = gpar(fill = "white"),
    straight = TRUE,
    as_label = TRUE
  )
  pth <- makeContent(pth)

  box2 <- pth$children[[2]]

  x1 <- as_inch(box1$x)
  x2 <- as_inch(box2$x)

  expect_lt(sum(abs(x1 - x2)), 2)

  y1 <- as_inch(box1$y)
  y2 <- as_inch(box2$y)

  expect_lt(sum(abs(y1 - y2)), 2)
})

test_that("radius is shrunk when needed", {
  pth <- textpathGrob(
    "ABC",
    x = c(2.5, 7.5),
    y = c(5, 5),
    id = c(1, 1),
    gp_box = gpar(fill = "white"),
    default.units = "in",
    label.r = unit(0.1, "inch"),
    label.padding = unit(0, "inch"),
    as_label = TRUE
  )
  attr(pth$textpath$label[[1]], "metrics")$height <- 0.2
  pth <- makeContent(pth)
  box1 <- pth$children[[2]]

  pth <- textpathGrob(
    "ABC",
    x = c(2.5, 7.5),
    y = c(5, 5),
    id = c(1, 1),
    gp_box = gpar(fill = "white"),
    default.units = "in",
    label.r = unit(1, "inch"),
    label.padding = unit(0, "inch"),
    as_label = TRUE
  )
  attr(pth$textpath$label[[1]], "metrics")$height <- 0.2
  pth <- makeContent(pth)
  box2 <- pth$children[[2]]

  expect_equal(as_inch(box1$x), as_inch(box2$x), tolerance = 1e-4)
  expect_equal(as_inch(box1$y), as_inch(box2$y), tolerance = 1e-4)
})


test_that("straight richtext is similar to richtext on straight path", {
  labels <- c(
    "A<span style='color:blue'>B</span>C",
    "D\nE<br>F"
  )
  x <- c(0, 1, 0, 1)
  y <- c(0, 1, 1, 0)
  id <- c(1, 1, 2, 2)


  ctrl <- textpathGrob(x = x, y = y, id = id,
                       label = labels, rich = TRUE,
                       default.units = "inch", as_label = TRUE)
  case <- textpathGrob(x = x, y = y, id = id,
                        label = labels, rich = TRUE, straight = TRUE,
                        default.units = "inch", as_label = TRUE)
  ctrl <- makeContent(ctrl)$children[[2]]
  case <- makeContent(case)$children[[2]]

  expect_equal(ctrl$gp, case$gp)
  expect_equal(ctrl$x, case$x, tolerance = 0.05)
  expect_equal(ctrl$y, case$y, tolerance = 0.05)
  expect_equal(ctrl$label, case$label)
})

test_that("We can set blank lines", {

  gp <- data_to_path_gp(data.frame(linetype = NA))
  expect_equal(gp$lty, 0)
})


test_that("We can remove labels too long for the path to support", {

  grob <- textpathGrob(label = "A label that is too long for its path",
                       x = c(0.45, 0.55), y = c(0.5, 0.5), id = c(1, 1),
                       default.units = "npc", remove_long = TRUE,
                       as_label = TRUE)
  grob <- makeContent(grob)
  expect_equal(class(grob$children[[1]])[1], "polyline")
})
