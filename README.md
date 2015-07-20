walkast
=======

related functions
-----------------

-   codetools::walkCode
-   pryr::ast (NSE wrapper of pryr::call\_tree)

``` r
library(pryr); library(codetools); library(walkast)
expr1 <- quote(sum(1, x + y, sin(z * w)))

walkCode(expr1)
#> sum
#> [1] 1
#> `+`
#> x
#> y
#> sin
#> `*`
#> z
#> w

call_tree(expr1)
#> \- ()
#>   \- `sum
#>   \-  1
#>   \- ()
#>     \- `+
#>     \- `x
#>     \- `y
#>   \- ()
#>     \- `sin
#>     \- ()
#>       \- `*
#>       \- `z
#>       \- `w

walk_ast(expr1)
#> List of 4
#>  $ : symbol sum
#>  $ : num 1
#>  $ :List of 3
#>   ..$ : symbol +
#>   ..$ : symbol x
#>   ..$ : symbol y
#>  $ :List of 2
#>   ..$ : symbol sin
#>   ..$ :List of 3
#>   .. ..$ : symbol *
#>   .. ..$ : symbol z
#>   .. ..$ : symbol w
```
