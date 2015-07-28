library(testthat)
context("walkast show")

test_that("walkast show", {

  e1 <- quote(1 + 2 * 3)
  expect_true(walk_ast(e1, show_lisp()) == "(+ 1 (* 2 3))")

  expect_true(walk_ast(e1, show_lisp(quote_bin = TRUE)) == "(`+` 1 (`*` 2 3))")

  expect_true(walk_ast(e1, show_r()) == "`+`(1, `*`(2, 3))")
})

