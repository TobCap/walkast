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
