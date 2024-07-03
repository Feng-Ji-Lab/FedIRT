mem <- function(f) {
  memo <- new.env(parent = emptyenv())
  function(...) {
    key <- paste(list(...), collapse = " ,")
    if(!exists(as.character(key), envir = memo)) {
      memo[[as.character(key)]] <- f(...)
    }
    memo[[as.character(key)]]
  }
}
