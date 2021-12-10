# rbind_dfs ---------------------------------------------------------------

test_that("rbind_dfs works", {

  df <- data_frame(x = 1:5, y = 2)
  df <- list(df, df)

  test <- rbind_dfs(df)
  expect_equal(dim(test), c(10, 2))
  expect_s3_class(test, "data.frame")

  test <- rbind_dfs(df, idcol = "id")
  expect_equal(test$id, rep(1:2, each = 5))

  names(df) <- c("A", "B")
  test <- rbind_dfs(df, idcol = "id")
  expect_equal(test$id, rep(c("A", "B"), each = 5))

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
  ctrl <- data_frame(x = x, y = y, id = id)

  # Should not remove row if unique
  test <- dedup_path(x, y, id)
  expect_equal(test, ctrl)

  # Should remove 2nd row but not 4th row
  y[2] <- 1
  test <- dedup_path(x, y, id)
  expect_equal(test, ctrl[c(1, 3:4),])

  # Check tolerance is respected
  y[2] <- 1.1
  test <- dedup_path(x, y, id, tolerance = 0.2)
  expect_equal(test, ctrl[c(1, 3:4), ])

  test <- dedup_path(x, y, id, tolerance = 0.05)
  expect_equal(test, transform(ctrl, y = c(y[1], 1.1, y[3:4])))
})

# approx_multiple ---------------------------------------------------------

test_that("approx_multiple works", {
  x <- 1:10
  xout <- c(2.5, 5, 7.5)

  # Zero-length input gives zero-length output
  y <- numeric()
  expect_equal(length(approx_multiple(x, xout, y)), 0L)

  # Single vector modus
  y <- (x - 5.5)^2
  test <- approx_multiple(x, xout, y)
  expect_equal(test, c(y[2] + y[3], 2*y[5], y[7] + y[8])/2)
  expect_type(test, "double")

  # Matrix modus
  y <- cbind(y1 = y, y2 = (x - 5.5)^3)
  test <- approx_multiple(x, xout, y)
  expect_equal(
    test,
    rbind(y[2, ] + y[3, ], 2 * y[5, ], y[7, ] + y[8, ]) / 2
  )
  expect_true(inherits(test, "matrix"))

  # Data frame modus
  y <- cbind(as.data.frame(y), dummy = "A")
  test <- approx_multiple(x, xout, y)
  expect_equal(
    test[1:2],
    rbind(y[2, 1:2] + y[3, 1:2], 2 * y[5, 1:2], y[7, 1:2] + y[8, 1:2]) / 2,
    ignore_attr = TRUE
  )
  expect_equal(test$dummy, y$dummy[1:3])
  expect_s3_class(test, "data.frame")

  # List modus
  ylist <- as.list(y)
  test  <- approx_multiple(x, xout, ylist)
  expect_equal(
    test[1:2],
    rbind(y[2, 1:2] + y[3, 1:2], 2 * y[5, 1:2], y[7, 1:2] + y[8, 1:2]) / 2,
    ignore_attr = TRUE
  )
  expect_equal(test$dummy, y$dummy[1:3])
  expect_s3_class(test, "data.frame")
})


# .interp_na

test_that("We can interpolate NA correctly", {

  expect_equal(.interp_na(c(1, 3, NA, 7)), c(1, 3, 5, 7))
  expect_error(.interp_na(c(NA, NA)))

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

test_that("run_end behaves as expected", {

  x <- 5:10

  expect_equal(run_end(x, is_lengths = TRUE), cumsum(x))
  expect_equal(run_end(x, is_lengths = FALSE), cumsum(run_len(x)))

})
