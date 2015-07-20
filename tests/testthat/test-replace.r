library(testthat)
context("walkast replace")

test_that("walkast replace", {
  expr1 <- quote(x + x + x)

  expect_identical(
    walk_ast(expr1, replace_pre(quote(x + x), quote(x)))
  , quote(x + x)
  )

  expect_identical(
    walk_ast(expr1, replace_post(quote(x + x), quote(x)))
    , quote(x)
  )
})
