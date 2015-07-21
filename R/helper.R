#' helper functions for walkast
#' @name helper
#' @examples
#' walk_ast(quote(1 + 2 * 3), to_list())
#' walk_ast(list(quote(`+`), 1, list(quote(`*`), 2, 3)), to_call())
#' walk_ast(quote(1 + 2 * 3), to_list() %then% to_call())
NULL

#' @rdname helper
#' @export
to_list <- function() make_visitor(
  call = function(x) if(!is.pairlist(x)) as.list(x) else x
)

#' @rdname helper
#' @export
to_call <- function() make_visitor(
  call = function(x) if(!is.pairlist(x)) as.call(x) else x
)

#' @rdname helper
#' @export
to_lisp <- function() make_visitor(
    leaf = function(x) as.character(deparse(x))
  , call = function(x) as_char(x)
  , vars = list(
      as_char = function(x) if (length(x) <= 1) hd(x) else paste0("(", hd(x[[1]]), " ", tl(x[-1]), ")")
    , hd = function(x) as.character(x)
    , tl = function(x) paste0(lapply(x, as_char), collapse = " ")
    , last = noquote
  )
)

to_r <- function() make_visitor(
    leaf = function(x) as.character(list(x))
  , call = function(x) as_char(x)
  , vars = list(
      as_char = function(x) if (length(x) <= 1) hd(x) else paste0(hd(x[[1]]), "(", tl(x[-1]), ")")
    , hd = function(x) as.character(list(x[[1]]))
    , tl = function(x) paste0(lapply(x, as_char), collapse = ", ")
    , last = noquote
    )
)