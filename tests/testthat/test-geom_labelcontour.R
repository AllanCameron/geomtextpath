test_that("We can produce labelled contour plots", {

  df   <- expand.grid(x = seq(nrow(volcano)), y = seq(ncol(volcano)))
  df$z <- as.vector(volcano)

  expect_silent(ggplot(df, aes(x, y, z = z)) + geom_labelcontour())
  expect_silent(ggplot(df, aes(x, y, z = z)) + stat_labelcontour())
})

test_that("We can produce labelled 2d density plots", {

  set.seed(1)

  df <- data.frame(x = rnorm(100), y = rnorm(100))

  expect_silent(ggplot(df, aes(x, y)) + geom_labeldensity2d())
})
