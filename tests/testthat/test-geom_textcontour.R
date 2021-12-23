test_that("We can produce labelled contour plots", {

  df   <- expand.grid(x = seq(nrow(volcano)), y = seq(ncol(volcano)))
  df$z <- as.vector(volcano)

  expect_silent(ggplot(df, aes(x, y, z = z)) + geom_textcontour())
  expect_silent(ggplot(df, aes(x, y, z = z)) + stat_textcontour())
  expect_silent(ggplot(df, aes(x, y, z = z)) + geom_labelcontour())

})

test_that("We can produce labelled 2d density plots", {

  set.seed(1)

  df <- data.frame(x = rnorm(100), y = rnorm(100))

  expect_silent(ggplot(df, aes(x, y)) + geom_textdensity2d())
})
