#' compose visitor functions
#'
#' @name compose_visitor
#' @param lhs visitor object
#' @param rhs visitor object
#' @return composed visitor object
#' @examples
#' add1 <- make_visitor(
#'   leaf = function(x) if(is.numeric(x)) x + 1 else x
#' )
#' mul2 <- make_visitor(
#'   leaf = function(x) if(is.numeric(x)) x * 2 else x
#' )
#' walk_ast(quote(1 + 2 * 3), add1)
#' walk_ast(quote(1 + 2 * 3), mul2)
#' walk_ast(quote(1 + 2 * 3), add1 %then% mul2)
#' walk_ast(quote(1 + 2 * 3), mul2 %then% add1)
NULL

#' @name compose_visitor
#' @export
`%then%` <- compose_visitor <- function(lhs, rhs) {
  stopifnot(is_visitor(lhs), is_visitor(rhs))

  e <- new.env(parent = parent.frame())

  list2env(list(
    handler = compose(lhs$handler, rhs$handler, e)
    , call = compose(lhs$call, rhs$call, e)
    , leaf = compose(lhs$leaf, rhs$leaf, e)
    , first = compose(lhs$first, rhs$first, e)
    , last = compose(lhs$last, rhs$last, e)
  ), envir = e)

  lhs_vars <- complement(lhs)
  rhs_vars <- complement(rhs)
  if (anyDuplicated(c(lhs_vars, rhs_vars)))
    stop("some user defined variable names are duplicated")

  if (length(lhs_vars) > 0) list2env(mget(lhs_vars, lhs), e)
  if (length(rhs_vars) > 0) list2env(mget(rhs_vars, rhs), e)

  `class<-`(e, "visitor")
}

# apply left to right
compose <- function(f, g, pf_ = parent.frame()) {
  f_id <- identical(f, identity, ignore.environment = TRUE)
  g_id <- identical(g, identity, ignore.environment = TRUE)

  environment(f) <- pf_
  environment(g) <- pf_

  if (f_id && g_id) identity
  else if (f_id && !g_id) f
  else if (!f_id && g_id) g
  else function(x) g(f(x))
}
complement <- function(x) {
  setdiff(ls(x, all.names = TRUE), c("handler", "call", "leaf", "first", "last"))
}
