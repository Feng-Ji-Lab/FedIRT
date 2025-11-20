# Memoization Function for Speed Optimization

A simple memoization function that stores the results of expensive
function calls and reuses those results when the same inputs occur
again. This technique greatly speeds up the computation of `fedirt`
function by caching previously computed values.

## Usage

``` r
mem(f)
```

## Arguments

- f:

  Function to be memd.

## Value

Returns a memd version of function `f` that will cache its previously
computed results for faster subsequent evaluations, especially
beneficial when applied to `fedirt`.

## Examples

``` r
# To mem a function, simply wrap it with `mem`:
mem(function(a,b){return(a+b)})
#> function (...) 
#> {
#>     key <- paste(list(...), collapse = " ,")
#>     if (!exists(as.character(key), envir = memo)) {
#>         memo[[as.character(key)]] <- f(...)
#>     }
#>     memo[[as.character(key)]]
#> }
#> <bytecode: 0x563b67079c60>
#> <environment: 0x563b68dcc8d0>
```
