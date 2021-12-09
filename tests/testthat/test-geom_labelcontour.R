test_that("We can produce labelled contour plots", {

  df   <- expand.grid(x = seq(nrow(volcano)), y = seq(ncol(volcano)))
  df$z <- as.vector(volcano)

  expect_silent(ggplot(df, aes(x, y, z = z)) + geom_labelcontour())

})
