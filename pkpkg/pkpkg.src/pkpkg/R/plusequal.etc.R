"%+=%" <- function(a,b) {eval.parent(substitute(a <- a + b)) }

"%-=%" <- function(a,b) {eval.parent(substitute(a <- a - b)) }

"%*=%" <- function(a,b) {eval.parent(substitute(a <- a * b)) }

"%/=%" <- function(a,b) {eval.parent(substitute(a <- a / b)) }

