Comparison of main features
---------------------------

|                                         |             codetools            |                           pryr                          |             walkast            |
|:---------------------------------------:|:--------------------------------:|:-------------------------------------------------------:|:------------------------------:|
|                 show ast                |         `walkCode(expr)`         |                    `call_tree(expr)`                    |  `walk_ast(expr, show_tree())` |
|               modify call               | `walkCode(expr, replaceLeaf)`[1] | `modify_call(call, new_args)`, `modify_lang(x, f, ...)` | `walk_ast(expr, replace(...))` |
|        print lisp-like expression       |         `showTree(expr)`         |                            NA                           |  `walk_ast(expr, show_lisp())` |
|           to list() structure           |    `walkCode(expr, toList)`[2]   |                            NA                           |   `walk_ast(expr, to_list())`  |
| is an expression object able to handle? |                No                |                           Yes                           |               No               |

Showing AST
-----------

``` r
library(pryr); library(codetools); library(walkast)
expr1 <- quote(sum(1, x + y, sin(z * w)))

walkCode(expr1)
```

    ## sum
    ## [1] 1
    ## `+`
    ## x
    ## y
    ## sin
    ## `*`
    ## z
    ## w

``` r
call_tree(expr1)
```

    ## \- ()
    ##   \- `sum
    ##   \-  1
    ##   \- ()
    ##     \- `+
    ##     \- `x
    ##     \- `y
    ##   \- ()
    ##     \- `sin
    ##     \- ()
    ##       \- `*
    ##       \- `z
    ##       \- `w

``` r
# using utils::str() to show expr structure
walk_ast(expr1, show_tree())
```

    ## List of 4
    ##  $ : symbol sum
    ##  $ : num 1
    ##  $ :List of 3
    ##   ..$ : symbol +
    ##   ..$ : symbol x
    ##   ..$ : symbol y
    ##  $ :List of 2
    ##   ..$ : symbol sin
    ##   ..$ :List of 3
    ##   .. ..$ : symbol *
    ##   .. ..$ : symbol z
    ##   .. ..$ : symbol w

[1] replaceLeaf \<- function(before, after) makeCodeWalker(leaf = function(e, w) if (identical(e, before)) after else e, call = function(e, w) as.call(lapply(e, walkCode, w)))

[2] toList \<- makeCodeWalker(call = function(e, w) lapply(e, walkCode, w), leaf = function(e, w) e)
