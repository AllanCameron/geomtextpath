cairo_pdf(tmp <- tempfile(fileext = ".pdf"))

test_that("We can measure plotmath expressions", {

  out <- measure_label(expression(cos(theta)))

  expect_true(abs(attr(out[[1]], "metrics")$width - 0.4) < 0.2)

  # Multiple expressions
  test_exp <- c(expression(cos(theta)), expression(sin(theta)))
  out <- measure_label(test_exp)

  expect_equal(length(out), 2L)
})

test_that("Composite glyphs are joined", {

  case <- "I a\u0302m co\u05aa\u05aamposed"
  ctrl <- "Simple string"

  test <- measure_label(ctrl)[[1]]
  expect_equal(nrow(test), nchar(ctrl))

  test <- measure_label(case)[[1]]
  expect_equal(nrow(test), nchar(case) - 3L)
})

test_that("Bidirectional text is flipped", {
  # I'm guessing default font on macOS doesn't have hebrew glyphs or something?
  skip_on_os("mac")

  case <- "Sarah is \u05e9\u05e8\u05d4 with \u05e9 on R"

  test <- measure_label(case)[[1]]
  test <- paste0(test$glyph, collapse = "")
  test <- utf8ToInt(test)
  test[test == 160] <- 32 # weird space / no-break space thing going on

  expect_equal(
    test,
    utf8ToInt("Sarah is \u05d4\u05e8\u05e9 with \u05e9 on R")
  )
})

test_that("Warn/error upon font fallback issues", {

  expect_error(measure_label("\u3053"), "No glyphs")
  expect_warning(measure_label(c("ABC", "\u3053")), "Not all glyphs")

})

dev.off()
unlink(tmp)
