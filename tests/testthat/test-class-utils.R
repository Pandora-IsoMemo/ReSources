context("Utility functions for object checking")

# nolint start

test_that("checkClass works as expected", {
  expect_error(
    ReSources:::checkClass(c(1, 2, 3), argName = "x"),
    "x has wrong type: numeric instead of list"
  )
  expect_error(
    ReSources:::checkClass(c(1:3)),
    "wrong type: integer instead of list"
  )
  expect_error(
    ReSources:::checkClass("c", classesExpected = "character"),
    NA
  )
  expect_error(ReSources:::checkClass(3, classesExpected = c("numeric", "integer")), NA)
  expect_error(ReSources:::checkClass(3L, classesExpected = c("numeric", "integer")), NA)
})


test_that("checkLength works as expected", {
  expect_error(
    ReSources:::checkLength(1:3, 2, argName = "x"),
    "x has wrong length: 3 instead of 2"
  )
  expect_error(
    ReSources:::checkLength(1:5, 3),
    "wrong length: 5 instead of 3"
  )
  expect_error(
    ReSources:::checkLength(1:3, "3"),
    NA
  )
  expect_error(
    ReSources:::checkLength(c("pli", "pla", "plup"), 3),
    NA
  )
})


test_that("checkNames works as expected", {
  aList <- list(
    x = 3,
    y = "pla",
    something = 4:8
  )
  expect_error(
    ReSources:::checkNames(aList, c("x", "y", "z"), argName = "aList"),
    "aList has wrong names: 'something, x, y' instead of 'x, y, z'"
  )
  expect_error(
    ReSources:::checkNames(aList, c("x", "y", "something")),
    NA
  )
})


test_that("checkDim works as expected", {
  x <- array(1:(3 * 4 * 5), dim = c(3, 4, 5))
  expect_error(ReSources:::checkDim(x, c(3, 4, 5)), NA)
  expect_error(
    suppressWarnings(ReSources:::checkDim(x, c(3, 4), argName = "x")),
    "x has wrong dimensions: '3, 4, 5' instead of '3, 4'"
  )
  expect_error(
    ReSources:::checkDim(x, c(1, 2, 3)),
    "wrong dimensions: '3, 4, 5' instead of '1, 2, 3'"
  )
})

# nolint end
