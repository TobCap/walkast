library(testthat)
context("walkast replace")

test_that("walkast replace", {
  expr1 <- quote(x + x + x)

  expect_identical(
      walk_ast(expr1, replace(quote(x + x), quote(x)))
    , quote(x)
  )

  expr2 <- quote(1 + 2 + x ^ 3)
  expect_identical(
      walk_ast(expr2, replace(quote(x), quote(y)))
    , quote(1+2+y^3)
  )

  expect_identical(
      walk_ast(expr2, replace(quote(x^3), quote(y)))
    , quote(1+2+y)
  )

  expect_identical(
      walk_ast(expr2, replace(quote(2), quote(99)))
    , quote(1+99+x^3)
  )

  expect_identical(
      walk_ast(expr2, replace(quote(`+`), quote(`-`)))
    , quote(1-2-x^3)
  )

  expect_identical(
      nest_expr(quote((1 + x)^2), quote(x), 2)
    , quote((1 + (1 + x)^2)^2)
  )

  expect_identical(
      nest_expr(quote((1 + x)^2), quote(x), 3)
    , quote((1 + (1 + (1 + x)^2)^2)^2)
  )

  expect_identical(
      nest_expr(quote((1 + x)^2), quote(1 + x), 2)
    , quote(((1 + x)^2)^2)
  )

  expect_identical(
      nest_expr(quote((1 + x)^2), quote(1 + x), 3)
    , quote((((1 + x)^2)^2)^2)
  )

  expect_identical(
      eval(nest_expr(quote(1 + 1 / x), quote(x), 40), list(x = 1))
    , (1 + sqrt(5)) / 2
  )

})
