#' replace call object
#' Replacement
#'
#' @param before language object or literal
#' @param after language object or literal
#' @param count count of iteration
#' @name replace
#' @examples
#' walk_ast(quote(x + y + z), replace_lang_top(quote(y), quote(www)))
#' walk_ast(quote(x + x + x), replace_lang_top(quote(x + x), quote(x)))
#' walk_ast(quote(x + x + x), replace_lang_bottom(quote(x + x), quote(x)))
#'
#' walk_ast(quote(1 + 2 + x^3), replace_lang_top(quote(x), quote(y)))
#' walk_ast(quote(1 + 2 + x^3), replace_lang_top(quote(x^3), quote(y)))
#' walk_ast(quote(1 + 2 + x^3), replace_lang_top(quote(2), quote(99)))
#' walk_ast(quote(1 + 2 + x^3), replace_lang_top(quote(1+2), quote(99)))
#' walk_ast(quote(1 + 2 + x^3), replace_lang_top(quote(`+`), quote(`-`)))
#'
#' nest_expr(quote((1 + x)^2), quote(x), 3)
#' nest_expr(quote((1 + x)^2), quote(1 + x), 3)
#' eval(nest_expr(quote(1 + 1 / x), quote(x), 40), list(x = 1)) == (1 + sqrt(5)) / 2
NULL

#' @export
#' @rdname replace
replace_lang_top <- function(before, after) {
  make_visitor(
    handler = function(x) if (identical(x, before)) function() after,
    vars = list(before = before, after = after) # need to capture vriables here
  )
}

#' @export
#' @rdname replace
replace_lang_bottom <- function(before, after) {
  make_visitor(
    call = function(x) if (identical(x, before)) after else x,
    vars = list(before = before, after = after)
  )
}

#' @export
#' @rdname replace
nest_expr <- function(before, after, count) {
  visitor <- make_visitor(
      handler = function(x) if (identical(x, after)) function() before
    , vars = list(before = before, after = after)
  )

  iter <- function(n, acc) {
    if (n == 1) acc
    else iter(n - 1, walk_ast(acc, visitor))
  }
  iter(count, before)
}
