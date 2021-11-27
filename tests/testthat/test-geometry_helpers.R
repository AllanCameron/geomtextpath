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
  br <- c(-0.15, 0.15)

  # TRUE cut_path
  test <- .get_surrounding_lines(xy, glyphs, cut_path = TRUE,
                                 breathing_room = br[2], vjust = vjust)
  expect_length(test$x, nrow(xy) * 2)
  expect_equal(
    test$x,
    c(1, 1.5 + br, 2,
      3, 3.5 + br, 4,
      5, 5.5 + br, 6)
  )
  expect_equal(unique(test$y), 1)

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
                                 breathing_room = br[2], vjust = vjust)
  expect_length(test$x, nrow(xy) + 2)
  expect_equal(
    test$x,
    c(1, 2,
      3, 3.5 + br, 4,
      5, 6)
  )
  expect_equal(unique(test$y), 1)

  # Test variable vjust is respected
  test <- .get_surrounding_lines(xy, glyphs, cut_path = NA, vjust = vjust,
                                 breathing_room = br[2], vjust_lim = c(0, 3))
  expect_length(test$x, nrow(xy) + 4)
  expect_equal(
    test$x,
    c(1, 1.5 + br, 2,
      3, 3.5 + br, 4,
      5, 6)
  )
  expect_equal(unique(test$y), 1)
})

test_that("text can be placed on 2-point paths", {
  # This is a canary in a coal-mine test to see if we haven't implemented
  # something that works for longer paths but not for very short paths.

  xy <- data.frame(x = c(1,2,3,4), y = c(1,2,2,1), id = c(1,1,2,2), size = 5)
  xy <- split(xy, xy$id)

  test <- Map(.get_path_points, label = c("A", "B"), path = xy)
  test <- rbind_dfs(test)

  # What actually to test is arbitrary, we just want the above to run without
  # errors and be notified if anything changes.
  expect_true(all(!is.na(test$x)))

})

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
