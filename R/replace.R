#' replace call object
#' Replacement
#'
#' @param before a language object or literal
#' @param after a language object or literal
#' @param expr a language object
#' @param target a langage object that you want to replace
#' @param count count of iteration
#'
#' @name replace
#' @examples
#' walk_ast(quote(x + y + z), replace(quote(y), quote(www)))
#' walk_ast(quote(x + x + x), replace(quote(x + x), quote(x)))
#'
#' walk_ast(quote(1 + 2 + x ^ 3), replace(quote(x), quote(y)))
#' walk_ast(quote(1 + 2 + x ^ 3), replace(quote(x^3), quote(y)))
#' walk_ast(quote(1 + 2 + x ^ 3), replace(quote(2), quote(99)))
#' walk_ast(quote(1 + 2 + x ^ 3), replace(quote(1+2), quote(99)))
#' walk_ast(quote(1 + 2 + x ^ 3), replace(quote(`+`), quote(`-`)))
#'
#' nest_expr(quote((1 + x)^2), quote(x), 3)
#' nest_expr(quote((1 + x)^2), quote(1 + x), 3)
#' eval(nest_expr(quote(1 + 1 / x), quote(x), 40), list(x = 1)) == (1 + sqrt(5)) / 2
NULL

#' @export
#' @rdname replace
replace <- function(before, after) {
  make_visitor(
      leaf = function(x) replace_(x)
    , call = function(x) replace_(x)
    , vars = list(
        replace_ = function(x) if (identical(x, before)) after else x
      , before = before
      , after = after)
  )
}

#' @export
#' @rdname replace
nest_expr <- function(expr, target, count) {
  visitor_replace <- replace(target, expr)
  iter <- function(n, acc) {
    if (n == 1) acc
    else iter(n - 1, walk_ast(acc, visitor_replace))
  }
  iter(count, expr)
}
