# check null infix
"%||%" <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

# check length 0 infix
"%0%" <- function(lhs, rhs) {
  if (length(lhs) == 0) rhs else length(lhs)
}

# possibly() otherwise NULL
possibly_null <- function(f, x){
  purrr::possibly(f, otherwise = NULL)(x)
}