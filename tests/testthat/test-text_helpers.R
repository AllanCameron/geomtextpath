png(tmp <- tempfile(fileext = ".png"))

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

  skip_on_cran()

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

test_that("richtext and parse are not set together", {

  expect_warning(static_text_params(rich = TRUE, parse = TRUE))
})

test_that("arbitrary parameters can be added to text params", {

  blah <- update_params(params = list(blah = "blah"))
  expect_equal(blah$blah, "blah")

  offset <- update_params(list(offset = 1))
  expect_equal(offset$text_params$offset, 1)
})

test_that("css parsing can take either type of quote", {

  expect_equal(parse_css_line("color:'blue'"), parse_css_line('color:"blue"'))
})

test_that("css units default to points if value of 0 given", {

  expect_equal(parse_css_unit("0"), list(value = 0, unit = "pt"))
})

dev.off()
unlink(tmp)
