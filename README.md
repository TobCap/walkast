{walkast}
=========

The objective
-------------

R is very flexble to handle a language object for meta-programming porpose. This package aims to provide a simple way to access a language object and you can manipulate it as you want by passing a visitor-function for printing, transforming, analyzing, and generating a new code.

OOP or Functional Style?
------------------------

It is natural for an OOP language to use visitor-pattern, and it is common for a functional language to use pattern-matching. R, I think, is not suitable for the use of those idea because semantics itself does not support those functionalities.

Related functions that already exist
------------------------------------

-   `codetools`::`walkCode` by [Luke Tierney](https://cran.r-project.org/web/packages/codetools/index.html)
-   `pryr`::`call_tree` (NSE version is `pryr`::`ast`) by [Hadley Wickham](https://cran.r-project.org/web/packages/pryr/index.html)

see [vignette](./vignettes/related_ast_functions.md) for a comparison among these functions.

Installation
------------

``` r
## install.packages("devtools")
devtools::install_github("tobcap/walkast")
library("walkast")
```

Main functions
--------------

#### `walk_ast(expr, visitor)`

-   expr: a language object (not permitted expression object)
-   visitor: S3 class that is an environment which must have six functions (leaf, call, hd, tl, first, last)

#### `make_visitor(leaf, call, hd, tl, first, last, vars)`

A helper which creates visitor-class.

-   leaf: a function that manipulates leaf part of langauge object (a symbol or an atomic)
-   call: a function that manipulates call part of langauge object (call object)
-   hd: a function that manipulates caller part (head of call object)
-   tl: a function that manipulates arguments part of call (tail of call object)
-   first: a function that manipulates expr before running AST
-   last: a function that manipulates expr after running AST

Other helper functions
----------------------

#### Printing

-   `show_tree`
-   `show_lisp`
-   `show_r`

#### Replacing

-   `replace`
-   `nest_expr`

#### Conversion

-   `to_list`
-   `to_call`

#### Combination

-   `%then%`

Examples
--------

``` r
library("walkast")
e1 <- quote(1+2)
```

``` r
walk_ast(e1)
#> 1 + 2
```

``` r
walk_ast(e1, show_tree())
#> List of 3
#>  $ : symbol +
#>  $ : num 1
#>  $ : num 2
```

``` r
mult2 <- make_visitor(leaf = function(x) if (is.numeric(x)) x * 2 else x)
walk_ast(e1, mult2)
#> 2 + 4

add1 <- make_visitor(leaf = function(x) if (is.numeric(x)) x + 1 else x)
walk_ast(e1, add1)
#> 2 + 3

walk_ast(e1, add1 %then% mult2)
#> 4 + 6
walk_ast(e1, mult2 %then% add1)
#> 3 + 5
```

``` r
walk_ast(e1, replace(quote(`+`), quote(`-`)))
#> 1 - 2
```

``` r
walk_ast(e1, replace(2, quote(x)))
#> 1 + x
```

``` r
e2 <- quote((1 + x) ^ 2)

nest_expr(e2, quote(x), 3)
#> (1 + (1 + (1 + x)^2)^2)^2

nest_expr(e2, quote(1 + x), 3)
#> (((1 + x)^2)^2)^2

nest_expr(quote(1 + 1 / x), quote(x), 5)
#> 1 + 1/(1 + 1/(1 + 1/(1 + 1/(1 + 1/x))))
```
