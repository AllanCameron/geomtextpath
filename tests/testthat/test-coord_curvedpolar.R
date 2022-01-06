# coord_curvedpolar tests

test_that("We can rescale x values to polar co-ordinates", {

  panel_params <- list(theta.range = c(0, 2))
  coord <- coord_curvedpolar()
  x <- c(0, 0.25, 0.75, 1, -Inf, Inf)
  theta <- theta_rescale(coord, x, panel_params)

  expect_equal(theta, c(0, pi / 4, 3 * pi / 4, pi, 0, 0))

})

test_that("We can create a grob output from coord_curvedpolar", {

  ccp <- coord_curvedpolar()
  pan <- list()
  thm <- theme(panel.border = element_rect())

  expect_true(grid::is.grob(ccp$render_fg(panel_params = pan, theme = thm)))

  # Make the labels for the axis, create a textpathGrob from the coord,
  # then recover the text from the textpathGrob

  pan <-  list(theta.range = c(0, 12),
               r.range = c(0, 5),
               theta.major = 1:12,
               theta.labels = c("I", "II", "III", "IV", "V", "VI", "VII",
                                "VIII", "IX", "X", "XI", "XII"))

  g_tree <- ccp$render_fg(panel_params = pan, theme = thm)

  tpg <- g_tree$children[[1]]

  lab <- sapply(tpg$textpath$label, function(x) paste(x$glyph, collapse = ""))

  expect_identical(lab, pan$theta.labels)
})

test_that("element_blank case is handled correctly", {

  p <- ggplot(data.frame(x = 1:10, y = 1:10), aes(x, y)) +
    geom_point() +
    coord_curvedpolar()

  out <- ggplot_gtable(ggplot_build(p))
  panel <- out$grobs[[which(out$layout$name == "panel")]]
  axis_labels <- panel$children[[5]]$children[[1]]

  expect_equal(class(axis_labels)[1], "textpath")

  out <- ggplot_gtable(ggplot_build(p + theme(axis.text.x = element_blank())))
  panel <- out$grobs[[which(out$layout$name == "panel")]]
  axis_labels <- panel$children[[5]]$children[[1]]

  expect_equal(class(axis_labels)[1], "zeroGrob")
})

test_that("wrapping first and last labels works as expected", {

  p <- ggplot(data.frame(x = 1:10, y = 1:10), aes(x, y)) +
    geom_point() +
    coord_curvedpolar()

  out <- ggplot_gtable(ggplot_build(p + scale_x_continuous(breaks = 1:10)))
  panel <- out$grobs[[which(out$layout$name == "panel")]]
  axis_labels <- panel$children[[5]]$children[[1]]

  expect_equal(axis_labels$textpath$label[[9]]$glyph, c("1", "/", "1", "0"))

  out <- ggplot_gtable(ggplot_build(p + scale_x_continuous(breaks = 1:10,
                                                      labels = as.expression)))
  panel <- out$grobs[[which(out$layout$name == "panel")]]
  axis_labels <- panel$children[[5]]$children[[1]]

  expect_identical(make_label(axis_labels$textpath$label[[9]]$glyph),
                   expression(paste(1, "/", 10)))
})
