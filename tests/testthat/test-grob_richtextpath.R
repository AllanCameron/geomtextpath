test_that("label can be missing", {

  case <- richtextpathGrob(x = c(0, 1), y = c(0, 1), id = c(1, 1))
  ctrl <- richtextpathGrob(x = c(0, 1), y = c(0, 1), id = c(1, 1),
                           label = "<i>A</i>B<b>C</b>")

  expect_null(case$textpath)
  expect_type(ctrl$textpath, "list")

  case <- makeContent(case)
  ctrl <- makeContent(ctrl)

  expect_s3_class(case, "zeroGrob")
  expect_s3_class(ctrl, "gTree")

  test <- richtextpathGrob(
    x = c(0, 1), y = c(0, 1), id = c(1, 1), label = "<i>A</i>B<b>C</b>",
    polar_params = list(x = 0.5, y = 0.5)
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

  gp <- gp_fill_defaults(gpar(), fontface = "plain")
  test <- parse_richtext(label, gp)
  expect_equal(test$font, c(3, 1, 2, 4))

  gp <- gp_fill_defaults(gpar(), fontface = "italic")
  test <- parse_richtext(label, gp)
  expect_equal(test$font, c(3, 3, 4, 4))

  gp <- gp_fill_defaults(gpar(), fontface = "bold")
  test <- parse_richtext(label, gp)
  expect_equal(test$font, c(4, 2, 2, 4))

  gp <- gp_fill_defaults(gpar(), fontface = "bold.italic")
  test <- parse_richtext(label, gp)
  expect_equal(test$font, c(4, 4, 4, 4))
})

test_that("unit vjust works", {
  case <- richtextpathGrob(x = c(0, 1), y = c(0, 1), id = c(1, 1),
                           label = "<i>A</i>B<b>C</b>", vjust = unit(1, "cm"))
  ctrl <- richtextpathGrob(x = c(0, 1), y = c(0, 1), id = c(1, 1),
                           label = "<i>A</i>B<b>C</b>", vjust = 0)
  # TODO: Implement straight method
  err  <- substitute(
    richtextpathGrob(x = c(0, 1), y = c(0, 1), id = c(1, 1),
                     label = "<i>A</i>B<b>C</b>", straight = TRUE)
  )

  case <- attr(case$textpath$label[[1]], "offset")
  ctrl <- attr(ctrl$textpath$label[[1]], "offset")

  expect_s3_class(case, "unit")
  expect_type(ctrl, "double")
  expect_error(eval(err), "doesn't work yet")
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


