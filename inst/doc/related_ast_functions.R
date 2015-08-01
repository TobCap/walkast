## ---- echo = TRUE, message = FALSE---------------------------------------
library(pryr); library(codetools); library(walkast)
expr1 <- quote(sum(1, x + y, sin(z * w)))

walkCode(expr1)

call_tree(expr1)

# using utils::str() to show expr structure
walk_ast(expr1, show_tree())

