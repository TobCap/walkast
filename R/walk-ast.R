#' walk ast
#'
#' @param expr a call object
#' @param visitor visitor class created by make_visitor()
#'
#' @param handler a function
#' @param call a function
#' @param leaf a function
#' @param first a function
#' @param last a function
#' @param vars list of variables that are used inside make_visitor()
#'
#' @name ast
#'
#' @examples
#' walk_ast(quote(x + y * z)) # default is show_tree()
#' walk_ast(quote(x + y * z), show_tree())
#' walk_ast(quote(x + y * z), make_visitor(call = as.list, last = str)) # the same as above
#'
#' time2_leaf <- make_visitor(
#'   leaf = function(x) if (is.numeric(x)) x * 2 else x
#' )
#' walk_ast(quote(1+2*3), time2_leaf)
#'
#' add_leaf <-
#' make_visitor(
#'   leaf = function(x) {if (is.numeric(x)) val <<- val + x},
#'   first = function(x) val <<- 0, # need to initialize
#'   last = function(x) val,
#'   vars = list(val = 0)
#' )
#'
#' count_leaf <-
#'   make_visitor(
#'     leaf = function(x) v <<- v + 1,
#'     first = function(x) v <<- 0,
#'     last = function(x) v,
#'     vars = list(v = 0)
#'   )
#' walk_ast(quote(1 + 2 * 3), add_leaf)
#' walk_ast(quote(1 + 2 * 3), count_leaf)
#' time2_leaf <- make_visitor(
#' leaf = function(x) if (is.numeric(x)) x * 2 else x
#' )
#' walk_ast(quote(1+2*3), time2_leaf)
NULL

#' @rdname ast
#' @export
walk_ast <- function(expr, visitor = show_tree()) {
  stopifnot(!is.expression(expr), is_visitor(visitor))
  v <- visitor

  rm_srcref <- function(x) {
    lst <- as.list(x)
    lst[!vapply(lst, inherits, FALSE, "srcref")]
  }

  iter <- function(x) {
    if (is.atomic(x) || is.symbol(x)) v$leaf(x)
    else if (is.pairlist(x)) as.pairlist(lapply(rm_srcref(x), iter))
    else v$call(as.call(c(list(v$hd(iter(x[[1]]))), v$tl(iter(as.pairlist(as.list(x)[-1]))))))
  }

  v$first(expr)
  ans <- iter(expr)
  v$last(ans)
}

#' @rdname ast
#' @export
make_visitor <- function(
      handler = function(x) NULL
    , call = identity
    , leaf = identity
    , first = identity
    , last = identity
    , hd = identity
    , tl = identity
    , vars = list()
  ) {
  stopifnot(is.list(vars))
  set_func(
    list(handler = handler, call = call, leaf = leaf, hd = hd, tl = tl, first = first, last = last)
  , environment())
  set_vars(vars)

  `class<-`(environment(), "visitor")
}

#' @rdname ast
#' @export
is_visitor <- function(visitor) is.environment(visitor) && inherits(visitor, "visitor")

#' @rdname ast
#' @export
show_tree <- function() make_visitor(call = as.list, last = str)

set_func <- function(fs, e) {
  for (i in seq_along(fs)) {
    f <- fs[[i]]
    if (typeof(f) == "closure" && !is_pkg_fun(f) && !identical(identity, f, ignore.environment = TRUE))
      assign(names(fs[i]), `environment<-`(f, e), envir = e)
  }
}

is_pkg_fun <- function(f) paste0("package:", environmentName(environment(f))) %in% search()

set_vars <- function(lst, pf_ = parent.frame()) {
  if (!is.list(lst)) stop("must be list")

  rm(list = as.character(substitute(lst)), envir = pf_)
  if (length(lst) == 0) return(invisible(NULL))

  lst_names <- names(lst)
  if (length(lst_names) == 0 || any(!nzchar(lst_names))) {
    stop("all needs to be named")
  }

  for(i in seq_along(lst)) {
    val <- lst[[i]]
    if (is.function(val) && !is.primitive(val))
      environment(val) <- pf_
    assign(lst_names[i], val, pf_)
  }

}
