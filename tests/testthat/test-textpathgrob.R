
test_that("textpathGrobs can be created", {

  grid.newpage()

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
  expect_silent(makeContent(textpathGrob(label = c("He", "Wo", "lorem", "ipsu"),
                             x = c(0, 1, 1.5, 2, 3, 4),
                             y = c(0, 1, 2, 1, 3, 4),
                             id = c(1, 1, 2, 2, 3, 4),
                             gp_path = gpar(lty = 1),
                             angle = 0,
                             polar_params = list(x = .5, y = .5, theta = "x"))))

  # Mixed points and paths with angles and unit polar parameters
  expect_silent({a <- textpathGrob(label = c("Hello", "World", "lorem", "ipsu"),
                             x = c(0, 1, 1.5, 2, 3, 4),
                             y = c(0, 1, 2, 1, 3, 4),
                             id = c(1, 1, 2, 2, 3, 4),
                             gp_path = gpar(lty = 1),
                             angle = 0,
                             polar_params = list(x = unit(.5, "in"),
                                                 y = unit(.5, "in"),
                                                 theta = "x"));
                  makeContent(a)})

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

  # Textpath grobs without a textpath member produce a zeroGrob

  b <- textpathGrob("b")
  b$textpath <- NULL
  res <- makeContent(b)
  expect_equal(class(res), c("zeroGrob", "grob", "gDesc"))


})

test_that("We can correctly pathify points", {

  data   <- data.frame(x = 0.75, y = 0.2, id = 1)

  # linear pathify
  linear <- pathify(data, hjust = 0.5, angle = 45, width = 1)
  # Polar pathify
  polar  <- pathify(data, hjust = 0.5, angle = 45, width = 1,
                    polar_x = 0.5, polar_y = 0.5, thet = "y")

  expect_equal(nrow(linear), 100L)
  expect_equal(nrow(polar), 100L)

  expect_true(abs(polar$x[1] - 0.2290784) < 1e-6)
  expect_true(abs(linear$x[1] - 0.3964466) < 1e-6)

})


test_that("We can add to default gpar", {

  expect_equal(gp_fill_defaults(gpar(size = 5))$size, 5L)
  expect_equal(gp_fill_defaults(gpar(size = 5), smell = 6)$smell, 6L)
})

# Rich text ---------------------------------------------------------------

test_that("label can be missing", {

  case <- textpathGrob(x = c(0, 1), y = c(0, 1), id = c(1, 1), rich = TRUE)
  ctrl <- textpathGrob(x = c(0, 1), y = c(0, 1), id = c(1, 1), rich = TRUE,
                           label = "<i>A</i>B<b>C</b>")

  expect_null(case$textpath)
  expect_type(ctrl$textpath, "list")

  case <- makeContent(case)
  ctrl <- makeContent(ctrl)

  expect_s3_class(case, "zeroGrob")
  expect_s3_class(ctrl, "gTree")

  test <- textpathGrob(
    x = c(0, 1), y = c(0, 1), id = c(1, 1), label = "<i>A</i>B<b>C</b>",
    polar_params = list(x = 0.5, y = 0.5), rich = TRUE
  )
  ppar <- test$textpath$params$polar_params
  expect_equal(convertUnit(ppar$x, "npc", valueOnly = TRUE), 0.5)
  expect_equal(convertUnit(ppar$y, "npc", valueOnly = TRUE), 0.5)

  expect_equal(test$textpath$label[[1]]$glyph, c("A", "B", "C"))
  expect_equal(test$textpath$gp_text$font, c(3, 1, 2))

})

test_that("richt text parsing works as expected", {
  gp <- gp_fill_defaults(gpar(), fontface = "plain")

  label <- "<em>italic</em><strong>bold</strong>"
  test <- parse_richtext(label, gpar())
  expect_equal(test$text, c("italic", 'bold'))
  expect_equal(test$font, c(3, 2))

  label <- "<notimplemented>my text here</notimplemented>"
  test <- substitute(parse_richtext(label, gpar()))
  expect_error(eval(test), "limited number of tags")

  label <- "<span style='color:blue;font-size:15pt;font-family:mono'>Test</span>"
  test <- parse_richtext(label, gp)
  expect_equal(test$fontfamily, "mono")
  expect_equal(test$fontsize, 15)
  expect_equal(test$col, "blue")
})


test_that("fontfaces are combined correctly", {
  # Note 1 = plain, 2 = bold, 3 = italic, 4 = bold.italic
  label <- "<i>A</i>B<b>C</b><i><b>D</b></i>"

  test <- parse_richtext(label, gpar(fontface = "plain"))
  expect_equal(test$font, c(3, 1, 2, 4))

  test <- parse_richtext(label, gpar(fontface = "italic"))
  expect_equal(test$font, c(3, 3, 4, 4))

  test <- parse_richtext(label, gpar(fontface = "bold"))
  expect_equal(test$font, c(4, 2, 2, 4))

  test <- parse_richtext(label, gpar(fontface = "bold.italic"))
  expect_equal(test$font, c(4, 4, 4, 4))
})

test_that("unit vjust works", {
  case <- textpathGrob(x = c(0, 1), y = c(0, 1), id = c(1, 1), rich = TRUE,
                       label = "<i>A</i>B<b>C</b>", vjust = unit(1, "cm"))
  ctrl <- textpathGrob(x = c(0, 1), y = c(0, 1), id = c(1, 1), rich = TRUE,
                       label = "<i>A</i>B<b>C</b>", vjust = 0)

  case <- attr(case$textpath$label[[1]], "offset")
  ctrl <- attr(ctrl$textpath$label[[1]], "offset")

  expect_s3_class(case, "unit")
  expect_type(ctrl, "double")
})

# These tests are mostly to hit the coverage
test_that("setup_context fills defaults", {
  ctxt <- setup_context()
  expect_s3_class(ctxt$gp, "gpar")
  expect_type(ctxt$yoff, "double")
})

test_that("css is parsed", {
  test <- parse_css("''")
  expect_equal(test, list())

  test <- parse_css("")
  expect_null(test)

  test <- parse_css("font-family:mono")
  expect_equal(test, list(`font-family` = "mono"))

  test <- parse_css("color:blue;font-family:mono")
  expect_equal(test, list(color = "blue", `font-family` = "mono"))

  u <- c("1px", "1in", "1cm", "1mm")
  u <- lapply(u, function(i){convert_css_unit_pt(i)})
  expect_equal(u, list(0.75, 72, 28.3, 2.8), tolerance = 0.1)

  err <- substitute(convert_css_unit_pt("1nonsense"))
  expect_error(eval(err), "Cannot convert")

  err <- substitute(convert_css_unit_pt(""))
  expect_error(eval(err), "valid CSS unit")
})
