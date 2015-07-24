walkast
=======

The objective
-------------

R is very flexble to handle a language object for meta-programming porpose. This package aims to provide a simple way to access a language object and you can manipulate it as you want by passing a visitor-function.

Related functions that already exist
------------------------------------

-   `codetools::walkCode`
-   `pryr::call_tree` (NSE version is `pryr::ast`)

see [vignette](./vignettes/related_ast_functions.html) for a comparison among these functions.

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("tobcap/walkast")
library("walkast")
```

Examples
--------
