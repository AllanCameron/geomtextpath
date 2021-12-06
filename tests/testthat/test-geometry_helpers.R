# Text angles -------------------------------------------------------------

test_that("Text angles are correct", {

  # Triangle
  xy <- data.frame(x = 1:5 * sqrt(2), y = c(1,2,3,2,1) * sqrt(2),
                   size = 5)

  angles    <- .angle_from_xy(xy$x, xy$y, degrees = TRUE)
  arclength <- .arclength_from_xy(xy$x, xy$y)

  # Test angles and lenghts of .add_path_data
  expect_equal(angles[1:2], c( 45,  45))
  expect_equal(angles[3:4], c(-45, -45))
  expect_equal(arclength, c(2 * 0:4))

  labels <- measure_text("O")[[1]]

  # measure_text can handle unit vjust
  expect_silent(measure_text("O", vjust = unit(0, "mm")))

  # Test angles of `.get_path_points`
  test <- .get_path_points(xy, labels, hjust = 0.25)
  expect_equal(test$angle[test$label != " "], 45)

  test <- .get_path_points(xy, labels, hjust = 0.75)
  expect_equal(test$angle[test$label != " "], -45)

  # This should be at the exact top of the triangle, where angle should be 0
  test <- .get_path_points(xy, labels, hjust = 0.5)
  expect_equal(test$angle[test$label != " "], 0)

  # Test location of letters
  expect_equal(test$x[test$label != " "], 3 * sqrt(2))

})


test_that("Appropriate warning with excess curvature", {

  t <- seq(0, 2 * pi, len = 100)
  xy <- data.frame(x = 0.01 * cos(t), y = 0.01 * sin(t), size = 5)

  angles    <- .angle_from_xy(xy$x, xy$y, degrees = TRUE)
  arclength <- .arclength_from_xy(xy$x, xy$y)

  labels <- measure_text("O", vjust = -4)[[1]]

  # Test angles of `.get_path_points`
  expect_warning(.get_path_points(xy, labels, hjust = 0.25),
                 "curvature")
})

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

test_that("We can have flat labels when requested", {

  df <- data.frame(x = seq(0, 2 * pi, length = 1000) / (2 * pi),
                   y = (sin(seq(0, 2 * pi, length = 1000)) + 1)/2,
                   z = rep(as.character(expression(sin(x))), 1000))

  grob <- textpathGrob(label = parse(text = df$z[1]),
                       x = df$x,
                       y = df$y,
                       id = rep(1, 1000),
                       vjust = 0.5)

  out <- makeContent(grob)

  expect_equal(as.character(out$children[[2]]$label), "sin(x)")


})

# Path trimming -----------------------------------------------------------

test_that("Path trimming is correct", {
  # Prep data
  xy <- data.frame(
    x = c(1:6), y = 1,
    id = c(1,1,2,2,3,3),
    size = 5,
    label = "a label"
  )
  vjust <- c(2, 0.5, -1)
  xy$group <- xy$id
  xy <- split(xy, xy$id)
  xy <- lapply(xy, function(x) {
    x$length <- .arclength_from_xy(x$x, x$y); x;})
  label  <- measure_text(c("A", "B", "C"))
  glyphs <- Map(.get_path_points, path = xy, label = label)
  glyphs <- rbind_dfs(glyphs)
  xy     <- rbind_dfs(xy)

  # Breathing room
  br <- 0.15
  lefts  <- sapply(label, function(x) x$xmid - x$xmin) + br
  rights <- sapply(label, function(x) x$xmax - x$xmid) + br

  # TRUE cut_path
  test <- .get_surrounding_lines(xy, glyphs, cut_path = TRUE,
                                 breathing_room = br, vjust = vjust)
  expect_length(test$x, nrow(xy) * 2)
  expect_equal(
    test$x,
    c(1, 1.5 - lefts[1], 1.5 + rights[1], 2,
      3, 3.5 - lefts[2], 3.5 + rights[2], 4,
      5, 5.5 - lefts[3], 5.5 + rights[3], 6)
  )
  expect_equal(unique(test$y), 1)

  # vjust can be passed as unit object
  expect_silent(.get_surrounding_lines(xy, glyphs, cut_path = TRUE,
                                 breathing_room = br, vjust = unit(0, "mm")))


  # FALSE cut_path
  test <- .get_surrounding_lines(xy, glyphs, cut_path = FALSE,
                                 breathing_room = br[2], vjust = vjust)
  expect_length(test$x, nrow(xy))
  expect_equal(
    test$x,
    c(1, 2,
      3, 4,
      5, 6)
  )
  expect_equal(unique(test$y), 1)

  # Variable cut_path
  test <- .get_surrounding_lines(xy, glyphs, cut_path = NA,
                                 breathing_room = br, vjust = vjust)
  expect_length(test$x, nrow(xy) + 2)
  expect_equal(
    test$x,
    c(1, 2,
      3, 3.5 - lefts[2], 3.5 + rights[2], 4,
      5, 6)
  )
  expect_equal(unique(test$y), 1)

  # Test variable vjust is respected
  test <- .get_surrounding_lines(xy, glyphs, cut_path = NA, vjust = vjust,
                                 breathing_room = br, vjust_lim = c(0, 3))
  expect_length(test$x, nrow(xy) + 4)
  expect_equal(
    test$x,
    c(1, 1.5 - lefts[1], 1.5 + rights[1], 2,
      3, 3.5 - lefts[2], 3.5 + rights[2], 4,
      5, 6)
  )
  expect_equal(unique(test$y), 1)



})

# Short paths -------------------------------------------------------------

test_that("text can be placed on 2-point paths", {
  # This is a canary in a coal-mine test to see if we haven't implemented
  # something that works for longer paths but not for very short paths.

  xy <- data.frame(x = c(1,2,3,4), y = c(1,2,2,1), id = c(1,1,2,2), size = 5)
  xy <- split(xy, xy$id)
  label <- measure_text(c("A", "B"))

  test <- Map(.get_path_points, label = label, path = xy)
  test <- rbind_dfs(test)

  # What actually to test is arbitrary, we just want the above to run without
  # errors and be notified if anything changes.
  expect_true(all(!is.na(test$x)))

})

# Anchor points -----------------------------------------------------------

test_that("Anchor point calculations are correct", {
  lens  <- cbind(0:5, 0:5 * 2)
  width <- 2

  grid <- expand.grid(halign = c("left", "center", "right"),
                      hjust = c(0, 0.5, 1),
                      stringsAsFactors = FALSE)

  test <- vapply(seq_len(nrow(grid)), function(i) {
    .anchor_points(lens, width, hjust = grid$hjust[i], halign = grid$halign[i])
  }, numeric(2))

  expect_equal(test[1, ], rep(c(0, 1.5, 3), each = 3))
  expect_equal(test[2, ], 0:8)
})

# Flipping ----------------------------------------------------------------

test_that("Flipping logic is correct", {

  label <- measure_text("ABC")[[1]]
  xy <- data_frame(x = 2:1, y = 1:2)
  angle <- .angle_from_xy(xy$x, xy$y, norm = TRUE, degrees = TRUE)
  angle <- rep(angle, nrow(label))

  # Should return NULL if we're not interested in flipping
  test <- .attempt_flip(xy, label, angle = angle, flip_inverted = FALSE)
  expect_null(test)

  # Should return data.frame on approved flip
  test <- .attempt_flip(xy, label, angle = angle, flip_inverted = TRUE)
  expect_equal(class(test), "list")

  # Angles should not be amenable to flip
  xy <- data_frame(x = 2:1, y = 2:1)
  angle <- .angle_from_xy(xy$x, xy$y, norm = TRUE, degrees = TRUE)
  angle <- rep(angle, nrow(label))

  test <- .attempt_flip(xy, label, angle = angle, flip_inverted = TRUE)
  expect_null(test)

  # Test if .get_path_points() also respects this
  xy <- data_frame(x = 2:1, y = 1:2)

  case <- .get_path_points(xy, label, flip_inverted = TRUE)
  expect_equal(case$angle, c(-45, -45, -45))

  ctrl <- .get_path_points(xy, label, flip_inverted = FALSE)
  expect_equal(case$angle, ctrl$angle - 180)
})

test_that("Flipping appropriately adjusts offset", {

  label <- measure_text(c("ABC"))[[1]]
  attr(label, "offset") <- 1

  xy <- data_frame(x = c(1, 0), y = 2)

  ctrl <- .get_path_points(xy, label, flip_inverted = FALSE)
  case <- .get_path_points(xy, label, flip_inverted = TRUE)

  expect_equal(ctrl$y, c(1, 1, 1))
  expect_equal(case$y, c(1, 1, 1))
})

test_that("Flipping leads to correctly clipped path", {

  label <- measure_text(c("ABCD"))[[1]]
  attr(label, "offset") <- 1

  xy <- data_frame(x = c(2, 0), y = 2)
  xy$length <- .arclength_from_xy(xy$x, xy$y)
  xy$id <- 1

  ctrl <- .get_path_points(xy, label, hjust = 0, flip_inverted = FALSE)
  case <- .get_path_points(xy, label, hjust = 0, flip_inverted = TRUE)

  # Should have reverse order
  expect_equal(ctrl$length, sort(ctrl$length, decreasing = FALSE))
  expect_equal(case$length, sort(case$length, decreasing = TRUE))

  case$id <- ctrl$id <- 1L

  ctrl <- .get_surrounding_lines(xy, ctrl)
  case <- .get_surrounding_lines(xy, case)


  # They aren't exactly equal due to letter spacing, but they should be similar
  expect_equal(case$x, ctrl$x, tolerance = 0.01)
})

# Absolute offset ---------------------------------------------------------

test_that("We can set a unit offset", {

  grob <- textpathGrob(
    label = "ABC",
    x = c(0, 1), y = c(1, 1),
    id = c(1, 1),
    vjust = unit(0.5, "inch"),
    default.units = "inch"
  )
  offset <- attr(grob$textpath$label[[1]], "offset")
  offset <- convertUnit(offset, "inches", valueOnly = TRUE)

  expect_equal(offset[1:2], c(0, 0.5))

  content <- makeContent(grob)
  txt <- content$children[[2]]

  expect_equal(convertUnit(txt$y, "inch", valueOnly = TRUE),
               rep(offset[3] + 1, 3))
})

