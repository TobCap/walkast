#' helper functions for walkast
#' @param quote_bin boolean: showing quoted binary operator or not
#' @name helper
#' @examples
#' walk_ast(quote(1 + 2 * 3), to_list())
#' walk_ast(list(quote(`+`), 1, list(quote(`*`), 2, 3)), to_call())
#' walk_ast(quote(1 + 2 * 3), to_list() %then% to_call())
#' walk_ast(quote(1 + 2 * 3), show_lisp())
#' walk_ast(quote(1 + 2 * 3), show_lisp(quote_bin = TRUE))
#' walk_ast(quote(1 + 2 * 3), show_r())
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
show_lisp <- function(quote_bin = FALSE) {
  make_visitor(
    call = function(x) paste0(x, collapse = "")
  , hd = function(x) paste0("(", if (length(x) <= 1) as.character(if (quote_bin) list(x) else x) else tl(x), " ")
  , tl = function(x) paste0(paste0(x, collapse = " "), ")", collapse = "")
  , vars = list(quote_bin = quote_bin)
  , last = noquote
  )
}
#' @rdname helper
#' @export
show_r <- function() {
  make_visitor(
    call = function(x) paste0(x, collapse = "")
  , hd = function(x) if (length(x) <= 1) as.character(list(x)) else tl(x)
  , tl = function(x) paste0("(", paste0(x, collapse = ", "), ")", collapse = "")
  , last = noquote
  )
}