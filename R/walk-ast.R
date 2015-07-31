#' walk ast
#'
#' @param expr a call object
#' @param visitor visitor class created by make_visitor()
#'
#' @param call a function that manipulates a call object
#' @param leaf a function that manipulates an atomic or a symbol object
#' @param hd a function that manipulates a function object
#' @param tl a function that manipulates parameters of a call object
#' @param initial a function that manipulates expr before running AST
#' @param final a function that manipulates expr after running AST
#' @param ... aribtrary functions or variables
#'
#' @name ast
#'
#' @examples
#' walk_ast(quote(x + y * z)) # default returns the initial argument itself
#' walk_ast(quote(x + y * z), show_tree())
#' walk_ast(quote(x + y * z), make_visitor(call = as.list, final = str)) # the same as above
#'
#' time2_leaf <- make_visitor(
#'   leaf = function(x) if (is.numeric(x)) x * 2 else x
#' )
#' walk_ast(quote(1+2*3), time2_leaf)
#'
#' add_leaf <-
#' make_visitor(
#'   leaf = function(x) {if (is.numeric(x)) val <<- val + x},
#'   initial = function(x) {val <<- 0; x}, # need to initialize
#'   final = function(`_`) val,
#'   val = 0
#' )
#'
#' count_leaf <-
#'   make_visitor(
#'     leaf = function(`_`) v <<- v + 1,
#'     initial = function(x) {v <<- 0; x},
#'     final = function(`_`) v,
#'     v = 0
#'   )
#' walk_ast(quote(1 + 2 * 3), add_leaf)
#' walk_ast(quote(1 + 2 * 3), count_leaf)
NULL

#' @rdname ast
#' @export
walk_ast <- function(expr, visitor = make_visitor()) {
  stopifnot(!is.expression(expr), is_visitor(visitor))
  v <- visitor

  # How should it be?
  rm_srcref <- function(x) {
    lst <- as.list(x)
    lst[!vapply(lst, inherits, FALSE, "srcref")]
  }

  iter <- function(x) {
    if (is.atomic(x) || is.symbol(x)) v$leaf(x)
    else if (is.pairlist(x)) as.pairlist(lapply(rm_srcref(x), iter))
    else v$call(as.call(c(list(v$hd(iter(x[[1]]))), v$tl(iter(as.pairlist(as.list(x)[-1]))))))
  }

  ans0 <- v$initial(expr)
  ans1 <- iter(ans0)
  ans2 <- v$final(ans1)
  ans2
}

#' @rdname ast
#' @export
make_visitor <- function(
      leaf = identity
    , call = identity
    , hd = identity
    , tl = identity
    , initial = identity
    , final = identity
    , ...
  ) {
  set_func(environment(), dots = list(...))
  `class<-`(environment(), "visitor")
}

#' @rdname ast
#' @export
is_visitor <- function(visitor)
  is.environment(visitor) && inherits(visitor, "visitor")

set_func <- function(e, dots) {
  # if `dots` has more than one length and not named, list2env() returns error
  list2env(dots, e)
  rm("...", envir = e)
  lst <- as.list(e)
  for (i in seq_along(lst)) {
    f <- lst[[i]]
    if (typeof(f) == "closure" && !is_pkg_fun(f) && !identical(identity, f, ignore.environment = TRUE))
      assign(names(lst[i]), `environment<-`(f, e), envir = e)
  }
}

is_pkg_fun <- function(f) paste0("package:", environmentName(environment(f))) %in% search()
