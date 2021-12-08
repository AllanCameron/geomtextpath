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
