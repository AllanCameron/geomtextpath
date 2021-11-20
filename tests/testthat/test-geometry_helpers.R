test_that("Text angles are correct", {

  # Triangle
  xy <- data.frame(x = 1:5, y = c(1,2,3,2,1))
  xy <- .add_path_data(xy)

  # Test angles and lenghts of .add_path_data
  expect_equal(xy$angle[1:2], c( 45,  45))
  expect_equal(xy$angle[4:5], c(-45, -45))
  expect_equal(xy$length, c(0, sqrt(2) * 1:4))

  # Test angles of `.get_path_points`
  test <- .get_path_points(xy, "O", hjust = 0.25)
  expect_equal(test$angle[test$label != " "], 45)

  test <- .get_path_points(xy, "O", hjust = 0.75)
  expect_equal(test$angle[test$label != " "], -45)

  # This should be at the exact top of the triangle, where angle should be 0
  test <- .get_path_points(xy, "O", hjust = 0.5)
  expect_equal(test$angle[test$label != " "], 0)

  # Test location of letters
  expect_equal(test$x[test$label != " "], 3)
  expect_equal(test$y[test$label != " "], 3)
})

test_that("Path trimming is correct", {
  # Prep data
  xy <- data.frame(
    x = c(1:6), y = 1,
    id = c(1,1,2,2,3,3),
    vjust = c(2, 2, 0.5, 0.5, -1, -1)
  )
  xy <- split(xy, xy$id)
  xy <- Map(.add_path_data, .data = xy)
  glyphs <- Map(.get_path_points, path = xy, label = c("A", "B", "C"))
  glyphs <- do.call(rbind.data.frame, c(glyphs, make.row.names = FALSE))
  xy     <- do.call(rbind.data.frame, c(xy, make.row.names = FALSE))

  # Breathing room
  br <- c(-0.15, 0.15)

  # TRUE cut_path
  test <- .get_surrounding_lines(xy, glyphs, cut_path = TRUE,
                                 breathing_room = br[2])
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
                                 breathing_room = br[2])
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
                                 breathing_room = br[2])
  expect_length(test$x, nrow(xy) + 2)
  expect_equal(
    test$x,
    c(1, 2,
      3, 3.5 + br, 4,
      5, 6)
  )
  expect_equal(unique(test$y), 1)

  # Test variable vjust is respected
  test <- .get_surrounding_lines(xy, glyphs, cut_path = NA,
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
