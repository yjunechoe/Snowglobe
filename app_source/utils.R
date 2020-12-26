# check null infix
"%||%" <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

# check length 0 infix
"%0%" <- function(lhs, rhs) {
  len_lhs <- length(lhs)
  if (len_lhs == 0) rhs else len_lhs
}

# possibly() otherwise NULL
possibly_null <- function(f,x){
  possibly(f, otherwise = NULL)(x)
}