# rbind_dfs ---------------------------------------------------------------

test_that("rbind_dfs works", {

  df <- data_frame(x = 1:5, y = 2)
  df <- list(df, df)

  test <- rbind_dfs(df)
  expect_equal(dim(test), c(10, 2))
  expect_s3_class(test, "data.frame")

})

# run_len -----------------------------------------------------------------

test_that("run_len works", {
  runvalue <- c("A", "B", "C")
  runlen   <- c(3, 2, 5)

  x <- rep(runvalue, runlen)

  test <- run_len(x)
  expect_equal(test, runlen)

  test <- run_len(numeric())
  expect_equal(test, 0)

  runvalue <- c("A", NA, "C")
  x <- rep(runvalue, runlen)

  test <- run_len(x)
  expect_equal(test, runlen)
})

# data_frame --------------------------------------------------------------

test_that("data_frame works", {

  test <- data_frame(x = 1:5, y = 1)
  expect_s3_class(test, "data.frame")
  expect_equal(dim(test), c(5, 2))

  test <- substitute(data_frame(x = 1:5, y = 1:2))
  expect_error(eval(test), "Elements must equal the number of rows or 1")

  test <- substitute(data_frame(1:5, 5:1))
  expect_error(eval(test), "Elements must be named")

})

# dedup_path --------------------------------------------------------------

test_that("dedup_path works", {
  x  <- c(1, 1, 2, 1)
  y  <- c(1, 2, 3, 1)
  id <- c(1, 1, 1, 1)
  ctrl <- data_frame(x = x, y = y,  id = id, line_x = x, line_y = y)

  # Should not remove row if unique
  test <- dedup_path(x, y, id, x, y)
  expect_equal(test, ctrl)

  # Should remove 2nd row but not 4th row
  y[2] <- 1
  test <- dedup_path(x, y, id, x, y)
  expect_equal(test, ctrl[c(1, 3:4), ])

  # Check tolerance is respected
  y[2] <- 1.1
  test <- dedup_path(x, y, id, x, y, tolerance = 0.2)
  expect_equal(test, ctrl[c(1, 3:4), ])

  test <- dedup_path(x, y, id, x, y, tolerance = 0.05)
  expect_equal(test, transform(ctrl, y = c(y[1], 1.1, y[3:4]),
                               line_y = c(y[1], 1.1, y[3:4])))
})

# approx_multi -----------------------------------------------------------------

test_that("approx_multi works", {
  x <- 1:10
  xout <- c(2.5, 5, 7.5)

  # Zero-length input gives zero-length output
  y <- numeric()
  expect_equal(length(approx_multi(x, y, xout)), 0L)

  # Single vector modus
  y <- (x - 5.5)^2
  test <- approx_multi(x, y, xout)
  expect_equal(test, c(y[2] + y[3], 2 * y[5], y[7] + y[8]) / 2)
  expect_type(test, "double")

  # Matrix modus
  y <- cbind(y1 = y, y2 = (x - 5.5)^3)
  test <- approx_multi(x, y, xout)
  expect_equal(
    test,
    rbind(y[2, ] + y[3, ], 2 * y[5, ], y[7, ] + y[8, ]) / 2
  )
  expect_true(inherits(test, "matrix"))

  # Data frame modus
  y <- cbind(as.data.frame(y), dummy = "A")
  test <- approx_multi(x, y, xout)
  expect_equal(
    test[1:2],
    rbind(y[2, 1:2] + y[3, 1:2], 2 * y[5, 1:2], y[7, 1:2] + y[8, 1:2]) / 2,
    ignore_attr = TRUE
  )
  expect_equal(test$dummy, y$dummy[1:3])
  expect_s3_class(test, "data.frame")

  # List modus
  ylist <- as.list(y)
  test  <- approx_multi(x, ylist, xout)
  expect_equal(
    test[1:2],
    rbind(y[2, 1:2] + y[3, 1:2], 2 * y[5, 1:2], y[7, 1:2] + y[8, 1:2]) / 2,
    ignore_attr = TRUE
  )
  expect_equal(test$dummy, y$dummy[1:3])
  expect_s3_class(test, "data.frame")
})


# interp_na

test_that("We can interpolate NA correctly", {

  expect_equal(interp_na(c(1, 3, NA, 7)), c(1, 3, 5, 7))
  expect_error(interp_na(c(NA, NA)))

})

# safe_parse

test_that("text is safely parsed to expressions", {

  expect_identical(safe_parse("x^2"), expression(x^2))
  expect_error(safe_parse("y = :x^2"), "unexpected ':'")
  expect_error(safe_parse(1), "`text` must be a character vector")
  expect_identical(safe_parse(""), expression(NA))

})

# is.multichar

test_that("We can identify flat components", {

expect_true(is.multichar(expression(a)))

expect_false(is.multichar(c("a", "b")))

expect_true(is.multichar(c("ab", "b")))

expect_true(is.multichar(c(expression(a), expression(b))))

expect_false(is.multichar(factor(LETTERS)))

expect_true(is.multichar(factor(month.name)))
})


# make_label

test_that("Labels can be created from expressions", {

  exp_list <- list(quote(sin(x)), quote(cos(x)))
  exp_vec <- expression(sin(x), cos(x))
  chars <- c("sin(x)", "cos(x)")

  expect_equal(make_label(chars), chars)
  expect_identical(make_label(exp_list), exp_vec)
  expect_equal(make_label(list(1, 2)), 1:2)
})

# Tailor arrow ------------------------------------------------------------

test_that("arrows are expanded correctly", {
  data <- data_frame(
    id = c(1L, 1L, 2L, 2L, 3L, 3L),
    new_id = c(1L, 2L, 3L, 4L, 5L, 5L),
    section = c("pre", "post", "pre", "post", "all", "all")
  )

  test <- tailor_arrow(data, arrow(ends = "last"))
  # Angle should be NA when section is 'pre'
  expect_equal(test$angle, c(NA, 30, NA, 30, 30))
  expect_equal(test$ends, rep(2L, 5))

  test <- tailor_arrow(data, arrow(ends = "first"))
  # Angle should be NA when section is 'post'
  expect_equal(test$angle, c(30, NA, 30, NA, 30))
  expect_equal(test$ends, rep(1L, 5))

  # Angles should be preserved, but ends should be set correctly
  test <- tailor_arrow(data, arrow(ends = "both"))
  expect_equal(test$angle, rep(30, 5))
  expect_equal(test$ends, c(1L, 2L, 1L, 2L, 3L))

  # Test that we can use a mix of ends
  test <- tailor_arrow(data, arrow(ends = c("first", "last", "first")))
  expect_equal(test$angle, c(30, NA, NA, 30, 30))
  expect_equal(test$ends, c(1L, 1L, 2L, 2L, 1L))
})


# Documentation -----------------------------------------------------------

# This is a snapshot test to warn us whenever there is a change in how the
# aesthetics are autoprinted.
test_that("No changes occurred in autodocumentation of aesthetics", {
  txt <- rd_aesthetics("geom", "textpath")
  expect_snapshot(txt)
})

test_that("Nonexisting label variants aren't documented", {
  GeomTextdummy <- ggproto("GeomTextdummy",  Geom, required_aes = "ABC")

  doc <- rlang::with_bindings(
    rd_aesthetics("geom", "textdummy"),
    GeomTextdummy = GeomTextdummy, .env = globalenv()
  )
  expect_false(any(grepl("DEF", doc)))

  GeomLabeldummy <- ggproto("GeomLabeldummy", GeomTextdummy,
                            required_aes = c("ABC", "DEF"))

  doc <- rlang::with_bindings(
    rd_aesthetics("geom", "textdummy"),
    GeomTextdummy = GeomTextdummy,
    GeomLabeldummy = GeomLabeldummy,
    .env = globalenv()
  )

  expect_true(any(grepl("DEF", doc)))

  doc <- rd_aesthetics("stat", "text_contour")
  expect_true(any(grepl("code\\{x\\}", doc)))
})

test_that("find_global() finds global functions", {
  # Should find because should be visible from here
  test <- find_global("geom_textpath", env = globalenv())
  expect_type(test, "closure")
  # Should find because should search namespace of geomtextpath
  test <- find_global("geom_textpath", env = emptyenv())
  expect_type(test, "closure")
  # Should not find
  test <- find_global("This is nonsense", env = globalenv())
  expect_null(test)
})

test_that("check_subclass works", {
  test <- check_subclass(GeomTextpath, "Geom")
  expect_s3_class(test, "GeomTextpath")

  test <- check_subclass("textpath", "Geom")
  expect_s3_class(test, "GeomTextpath")

  test <- substitute(check_subclass("nonsense", "Geom"))
  expect_error(eval(test), "Can't find `geom`")

  test <- substitute(check_subclass(12, "Geom"))
  expect_error(eval(test), "must be either a string")
})


# This is a snapshot test to warn us whenever there is a change in how the
# dot argument is autoprinted.

test_that("rd_dots works as before", {
  file <- system.file("R", "utils.R", package = "geomtextpath")
  skip_if_not(file.exists(file), message = "utils.R has been moved")
  skip_if_not(requireNamespace("roxygen2", quietly = TRUE),
              message = "roxygen2 is not installed")

  txt <- rd_dots(geom_textsegment)
  expect_snapshot(txt)
})


# Parameters --------------------------------------------------------------

test_that("static_text_params asserts correctly", {

  test <- static_text_params(offset = NULL)
  expect_null(test$offset)

  test <- static_text_params(offset = unit(1, "npc"))
  expect_s3_class(test$offset, "unit")

  # Check error messages
  test <- substitute(static_text_params(halign = "top"))
  expect_error(eval(test), c('"center", "left", or "right"'))

  test <- substitute(static_text_params(text_only = 3))
  expect_error(eval(test), "must be a `logical` vector")

  test <- substitute(static_text_params(text_only = NA))
  expect_error(eval(test), "contains NAs whereas it cannot")

  test <- substitute(static_text_params(text_only = c(TRUE, FALSE)))
  expect_error(eval(test), "must be of length 1.")

  # Check defaults are correctly resolved
  test <- static_text_params("text")
  expect_equal(test$gap, NA)

  test <- static_text_params("label")
  expect_equal(test$gap, FALSE)
})


# Matching labels to vector & data frames --------------------------------------

test_that("We can match labels to vectors and data frames", {

  one_lab <- "label"
  two_labs <- c("label1", "label2")
  three_labs <- c("label1", "label2", "label3")
  df <- data.frame(x = 1:3, y = 1:3)
  expect_equal(match_labels(df, one_lab), rep("label", 3))
  expect_equal(match_labels(df, three_labs), three_labs)
  expect_error(match_labels(df, two_labs))
  expect_equal(match_labels(1:3, one_lab), rep("label", 3))
  expect_equal(match_labels(1:3, three_labs), three_labs)
  expect_error(match_labels(1:3, two_labs))

})

# Functions taken from ggplot2 -------------------------------------------------

test_that("We can find missing cases", {

  x <- c(1, NA, 3)
  y <- list(1, NULL, 3)
  z <- c(1, Inf, 3)
  res <- c(TRUE, FALSE, TRUE)
  res2 <- c(TRUE, FALSE, FALSE)
  df <- data.frame(x = x, y = c(1, 2, NA), z = z)

  expect_equal(res, is_missing(x))
  expect_equal(res, is_missing(y))
  expect_equal(res, is_finite(z))
  expect_equal(res, is_finite(y))
  expect_equal(res2, cases(df, is_missing))
  expect_equal(TRUE, cases(df[1, ], is_missing))
  expect_equal(res2, !detect_missing(df, c("x", "y")))
  expect_equal(res, !detect_missing(df, c("x", "z"), TRUE))
  expect_equal(!res2, find_missing(df, ggplot2::GeomPoint))
})


test_that("Objects are renamed correctly", {
  df1 <- data.frame(x = 1:3, y = 4:6)
  df2 <- data.frame(a = 1:3, b = 4:6)
  vec <- c(x = "a", y = "b", z = "c")
  expect_equal(c("a", "b"), names(rename(df1, vec)))
  expect_equal(c("a", "b"), names(rename(df2, vec)))
})


test_that("rd_dots can evaluate a function's dots", {

  expect_equal(substr(rd_dots("geom_textpath"), 1, 10), "@param ...")
})

test_that("resolution_to_unit works", {
  x <- resolution_to_unit(unit = "cm")
  y <- resolution_to_unit()
  expect_equal(x / 2.54, y)
})

test_that("We can warn about multiple overwritten arguments", {
  expect_warning(warn_overwritten_args("random_function", "overwritten",
                   c("parameter1", "parameter2", "parameter3")))
})
