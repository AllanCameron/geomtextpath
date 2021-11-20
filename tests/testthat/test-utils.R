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

test_that("data_frame works", {

  test <- data_frame(x = 1:5, y = 1)
  expect_s3_class(test, "data.frame")
  expect_equal(dim(test), c(5, 2))

  test <- substitute(data_frame(x = 1:5, y = 1:2))
  expect_error(eval(test), "Elements must equal the number of rows or 1")

  test <- substitute(data_frame(1:5, 5:1))
  expect_error(eval(test), "Elements must be named")

})
