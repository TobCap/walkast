library(testthat)
context("walkast %then%")

test_that("walkast %then%", {

  keep_source <- options()$keep.source
  options(keep.source = FALSE)
  on.exit(options(keep.source = keep_source))

  add1 <- make_visitor(
    leaf = function(x) if(is.numeric(x)) x + 1 else x
  )
  mul2 <- make_visitor(
    leaf = function(x) if(is.numeric(x)) x * 2 else x
  )

  e1 <- quote(1 + 2 * 3)
  expect_identical(walk_ast(e1, add1), quote(2 + 3 * 4))
  expect_identical(walk_ast(e1, mul2), quote(2 + 4 * 6))
  expect_identical(walk_ast(e1, add1 %then% mul2), quote(4 + 6 * 8))
  expect_identical(walk_ast(e1, mul2 %then% add1), quote(3 + 5 * 7))

})

