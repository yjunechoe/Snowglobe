# check NULL infix
"%||%" <- function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

# check length 0 infix
"%0%" <- function(lhs, rhs) {
  if (length(lhs) == 0) rhs else length(lhs)
}

# check NA infix
"%NA%" <- function(lhs, rhs) {
  if (is.na(lhs)) rhs else lhs
}

# check ID infix (from raw output)
"%!Id%" <- function(lhs, rhs) {
  if (is.null(lhs[["Id"]])) rhs else lhs
}

# possibly() otherwise NULL
possibly_null <- function(f, x){
  purrr::possibly(f, otherwise = NULL)(x)
}

# split long lines of text for plotting
text_split <- function(txt, len = 50) {
  
  as.character(txt) %>% 
    purrr::map_chr(~ {
      
      if (stringr::str_count(.x, " ") == 1 && stringr::str_length(.x) > len/2) {
        
        stringr::str_replace(.x, " ", "\n")
        
      } else if (stringr::str_length(.x) > len/2) {
        
        if (stringr::str_length(.x) > len) {
          split_txt <- .x %>% 
            stringr::str_sub(end = len) %>% 
            paste0("...") %>% 
            stringr::str_split(" ") %>% 
            `[[`(1)
        } else {
          split_txt <- stringr::str_split(.x, " ")[[1]]
        }

        break_index <- split_txt %>% 
          stringr::str_count() %>% 
          purrr::accumulate(`+`) %>% 
          `-`(len/2) %>% 
          abs() %>% 
          which.min() %>% 
          min(length(split_txt) - 1)
        
        paste(c(split_txt[1:break_index], "\n", split_txt[(break_index+1):length(split_txt)]), collapse = " ") %>% 
          stringr::str_replace(" \n ", "\n")
        
      } else {
        .x
      }
      
    })
  
}
