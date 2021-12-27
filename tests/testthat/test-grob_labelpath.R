test_that("label can be missing", {

  case <- labelpathGrob(x = c(0, 1), y = c(0, 1), id = c(1, 1))
  ctrl <- labelpathGrob(x = c(0, 1), y = c(0, 1), id = c(1, 1), label = "ABC")

  expect_null(case$textpath)
  expect_type(ctrl$textpath, "list")

  case <- makeContent(case)
  ctrl <- makeContent(ctrl)

  expect_s3_class(case, "zeroGrob")
  expect_s3_class(ctrl, "gTree")

  # Test that polar parameters are converted
  test <- labelpathGrob(
    x = c(0, 1), y = c(0, 1), id = c(1, 1), label = "ABC",
    polar_params = list(x = 0.5, y = 0.5)
  )
  ppar <- test$textpath$params$polar_params
  expect_equal(convertUnit(ppar$x, "npc", valueOnly = TRUE), 0.5)
  expect_equal(convertUnit(ppar$y, "npc", valueOnly = TRUE), 0.5)

})


test_that("straight and curved setting produce similar boxes", {

  pth <- labelpathGrob(
    "ABC",
    x = c(0, 1),
    y = c(0, 1),
    id = c(1, 1),
    gp_box = gpar(fill = "white")
  )
  pth <- makeContent(pth)

  box1 <- pth$children[[2]]

  pth <- labelpathGrob(
    "ABC",
    x = c(0, 1),
    y = c(0, 1),
    id = c(1, 1),
    gp_box = gpar(fill = "white"),
    straight = TRUE
  )
  pth <- makeContent(pth)

  box2 <- pth$children[[2]]

  x1 <- as_inch(box1$x)
  x2 <- as_inch(box2$x)

  expect_lt(sum(abs(x1 - x2)), 1)

  y1 <- as_inch(box1$y)
  y2 <- as_inch(box2$y)

  expect_lt(sum(abs(y1 - y2)), 1)
})

test_that("radius is shrunk when needed", {
  pth <- labelpathGrob(
    "ABC",
    x = c(2.5, 7.5),
    y = c(5, 5),
    id = c(1, 1),
    gp_box = gpar(fill = "white"),
    default.units = "in",
    label.r = unit(0.1, "inch"),
    label.padding = unit(0, "inch")
  )
  attr(pth$textpath$label[[1]], "metrics")$height <- 0.2
  pth <- makeContent(pth)
  box1 <- pth$children[[2]]

  pth <- labelpathGrob(
    "ABC",
    x = c(2.5, 7.5),
    y = c(5, 5),
    id = c(1, 1),
    gp_box = gpar(fill = "white"),
    default.units = "in",
    label.r = unit(1, "inch"),
    label.padding = unit(0, "inch")
  )
  attr(pth$textpath$label[[1]], "metrics")$height <- 0.2
  pth <- makeContent(pth)
  box2 <- pth$children[[2]]

  expect_equal(as_inch(box1$x), as_inch(box2$x), tolerance = 1e-4)
  expect_equal(as_inch(box1$y), as_inch(box2$y), tolerance = 1e-4)
})


test_that("straight richtext is similar to 'curved' richtext on straight path", {
  labels <- c(
    "A<span style='color:blue'>B</span>C",
    "D\nE<br>F"
  )
  x <- c(0, 1, 0, 1)
  y <- c(0, 1, 1, 0)
  id <- c(1, 1, 2, 2)


  ctrl <- labelpathGrob(x = x, y = y, id = id,
                       label = labels, rich = TRUE,
                       default.units = "inch")
  case <- labelpathGrob(x = x, y = y, id = id,
                        label = labels, rich = TRUE, straight = TRUE,
                        default.units = "inch")
  ctrl <- makeContent(ctrl)$children[[2]]
  case <- makeContent(case)$children[[2]]

  expect_equal(ctrl$gp, case$gp)
  expect_equal(ctrl$x, case$x)
  expect_equal(ctrl$y, case$y)
  expect_equal(ctrl$label, case$label)
})
