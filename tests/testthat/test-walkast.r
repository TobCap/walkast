library(testthat)
context("walkast default")

test_that("walkast default", {

  keep_source <- options()$keep.source
  options(keep.source = FALSE)
  on.exit(options(keep.source = keep_source))

  e1 <- quote(1 + sin(x ^ 3))
  expect_identical(walk_ast(e1, make_visitor()), e1)

  e2 <- quote(function(x = 3) x + 1)
  expect_equivalent(walk_ast(e2,make_visitor()), e2)

  time2_leaf <- make_visitor(
   leaf = function(x) if (is.numeric(x)) x * 2 else x
  )
  expect_equivalent(walk_ast(quote(1+2*3), time2_leaf), quote(2+4*6))
  expect_equivalent(walk_ast(e1, time2_leaf), quote(2+sin(x^6)))

})

