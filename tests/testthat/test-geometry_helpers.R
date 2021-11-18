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

