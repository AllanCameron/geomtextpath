test_that("hjust scales have correct guides and palettes", {

  shd <- scale_hjust_discrete(range = c(0, 0.5))
  expect_equal(shd$guide, "none")
  expect_equal(shd$aesthetics, "hjust")
  expect_equal(shd$palette(3), (0:2) / 4)

  shi <- scale_hjust_identity()
  expect_equal(shi$guide, "none")
  expect_equal(shi$aesthetics, "hjust")
  expect_equal(shi$palette(3), 3)

  shm <- scale_hjust_manual(values = c(0.1, 0.5, 0.9),
                            breaks = c(0, 5, 10))
  shm2 <- scale_hjust_manual(values = c(0.1, 0.5, 0.9),
                            breaks = c(0, 5, 8, 10))
  expect_equal(shm$guide, "none")
  expect_equal(shm$aesthetics, "hjust")
  expect_equal(shm$palette(1), c("0" = 0.1, "5" = 0.5, "10" = 0.9))
  expect_equal(names(shm2$palette(1)), c("0", "5", "8"))
  expect_error(shm$palette(4), "Insufficient values")
})

test_that("vjust scales have correct guides and palettes", {

  svd <- scale_vjust_discrete(range = c(0, 0.5))
  expect_equal(svd$guide, "none")
  expect_equal(svd$aesthetics, "vjust")
  expect_equal(svd$palette(3), (0:2) / 4)

  svi <- scale_vjust_identity()
  expect_equal(svi$guide, "none")
  expect_equal(svi$aesthetics, "vjust")
  expect_equal(svi$palette(3), 3)

  svm <- scale_vjust_manual(values = c(0.1, 0.5, 0.9))
  expect_equal(svm$guide, "none")
  expect_equal(svm$aesthetics, "vjust")
  expect_equal(svm$palette(1), c(0.1, 0.5, 0.9))
  expect_error(svm$palette(4), "Insufficient values")
})
