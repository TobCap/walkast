library(testthat)
context("walkast replace")

test_that("walkast replace", {
  expr1 <- quote(x + x + x)

  expect_identical(
    walk_ast(expr1, replace_lang_top(quote(x + x), quote(x)))
  , quote(x + x)
  )

  expect_identical(
    walk_ast(expr1, replace_lang_bottom(quote(x + x), quote(x)))
    , quote(x)
  )
})
