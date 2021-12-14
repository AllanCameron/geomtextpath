cairo_pdf()

test_that("We can measure plotmath expressions", {

  out <- measure_exp(expression(cos(theta)))

  expect_true(abs(attr(out[[1]], "metrics")$width - 0.4) < 0.2)

  # Multiple expressions
  test_exp <- c(expression(cos(theta)), expression(sin(theta)))
  out <- measure_exp(test_exp)

  expect_equal(length(out), 2L)

  gp <- gpar(fontsize = c(3, 3, 3))

  expect_error(measure_exp(test_exp, gp = gp), "fontsize")
})

test_that("Composite glyphs are joined", {

  case <- "I a\u0302m co\u05aa\u05aamposed"
  ctrl <- "Simple string"

  test <- measure_text(ctrl)[[1]]
  expect_equal(nrow(test), nchar(ctrl))

  test <- measure_text(case)[[1]]
  expect_equal(nrow(test), nchar(case) - 3L)
})

test_that("Bidirectional text is flipped", {
  # I'm guessing default font on macOS doesn't have hebrew glyphs or something?
  skip_on_os("mac")

  case <- "Sarah is \u05e9\u05e8\u05d4 with \u05e9 on R"

  test <- measure_text(case)[[1]]
  test <- paste0(test$glyph, collapse = "")
  test <- utf8ToInt(test)
  test[test == 160] <- 32 # weird space / no-break space thing going on

  expect_equal(
    test,
    utf8ToInt("Sarah is \u05d4\u05e8\u05e9 with \u05e9 on R")
  )
})

test_that("Warn/error upon font fallback issues", {

  expect_error(measure_text("\u3053"), "No glyphs")
  expect_warning(measure_text(c("ABC", "\u3053")), "Not all glyphs")

})

dev.off()
withr::defer(unlink("Rplot001.pdf"))
