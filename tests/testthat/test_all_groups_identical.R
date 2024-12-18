library(groupdata2)
context("all_groups_identical()")

test_that("groups are correctly compared with all_groups_identical()", {
  x1 <- factor(rep(c(1, 2, 3, 4, 5), each = 5))
  x2 <- factor(rep(rev(c(1, 2, 3, 4, 5)), each = 5))

  expect_true(all_groups_identical(x1, x2))

  x3 <- factor(rep(rev(c(1, 2, 3, 4, 6)), each = 5))

  expect_true(all_groups_identical(x1, x3))

  x4 <- factor(rep(rev(c("a", "b", "c", "d", "g")), each = 5))

  expect_true(all_groups_identical(x1, x4))

  xpectr::set_test_seed(1)
  x5 <- factor(sample(rep(rev(c("a", "b", "c", "d", "g")), each = 5)))
  xpectr::set_test_seed(1)
  x6 <- sample(factor(rep(rev(c("a", "b", "c", "d", "g")), each = 5)))
  expect_true(all_groups_identical(x5, x6))
  expect_true(!all_groups_identical(x1, x5))
  expect_true(!all_groups_identical(x1, x6))

  x7 <- c(1, 2, 3, 1, 2, 3, 1, 2, 3)
  x7_f <- factor(c(1, 2, 3, 1, 2, 3, 1, 2, 3))
  expect_true(all_groups_identical(x7, x7_f))

  x8 <- c(1, 2, 1, 3, 2, 3, 1, 2, 3)
  expect_true(!all_groups_identical(x7, x8))

  # x7 rolled/shifted
  x9 <- c(2, 3, 1, 2, 3, 1, 2, 3, 1)
  expect_true(all_groups_identical(x7, x9))

  x10 <- c(1, 3, 1, 2, 3, 1, 2, 3, 1)
  expect_true(!all_groups_identical(x7, x10))

  x11 <- c(2, 2, 3, 1, 2, 3, 1, 2, 3)
  x12 <- c(1, 2, 3, 1, 2, 3, 1, 2, 2)
  expect_true(!all_groups_identical(x7, x11))
  expect_true(!all_groups_identical(x7, x12))
  expect_true(!all_groups_identical(x11, x12))

  x13 <- rep(c(1.2, 3.4, 2.2), each = 3)
  x14 <- rep(c(3.4, 2.2, 1.2), each = 3)
  expect_true(all_groups_identical(x13, x14))
  x13_f <- factor(rep(c(1.2, 3.4, 2.2), each = 3))
  x14_f <- factor(rep(c(3.4, 2.2, 1.2), each = 3))
  expect_true(all_groups_identical(x13_f, x14_f))
  expect_true(all_groups_identical(x13, x13_f))

  x15 <- factor(c(1, 1, 2, 2, 3, 3, 4, 4))
  x16 <- factor(c(1, 1, 2, 3, 4, 5, 5, 5))
  x17 <- factor(c(2, 2, 3, 3, 4, 4, 5, 5))
  x18 <- factor(c(2, 2, 2, 2, 3, 3, 4, 4))

  expect_true(all_groups_identical(x15, x17))
  expect_true(all_groups_identical(x17, x15)) # Order could matter
  expect_true(!all_groups_identical(x15, x16))
  expect_true(!all_groups_identical(x16, x15))
  expect_true(!all_groups_identical(x15, x18))
  expect_true(!all_groups_identical(x18, x15))

  # Previous bug
  x19<-c(1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,2)
  x20<-c(1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,4)
  expect_true(!all_groups_identical(x19, x20))
  expect_true(!all_groups_identical(x20, x19)) # Different order used to fail

  x21<-c(1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,2)
  x22<-c(1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,1)
  expect_true(!all_groups_identical(x21, x22))
  expect_true(!all_groups_identical(x22, x21))

  x23<-c(1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,5)
  x24<-c(1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,4)
  expect_true(all_groups_identical(x23, x24))
  expect_true(all_groups_identical(x24, x23))


})
