is_empty <- function(x){
  (length(x) == 0L)
}


apply_na_padding <- function(data,n){
  max_len <- max(n)
  length(data) <- max_len
  return(data)
}
