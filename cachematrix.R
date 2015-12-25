## Functions below can be used to speed up matrix inversion calculations. 
## They use closures to cache inversion results.

## Function cretates a closure to manipulate square matrix
## with cached inverse matix.

makeCacheMatrix <- function(x = matrix()) {
  # Initialize cache
  solved <- NULL
  # set and get matrix
  set <- function(y) {
    x<<-y
    solved<<-NULL
  }
  get <- function() x
  # access cached result
  setsolved <- function(m) solved<<-m
  getsolved <- function() solved
  # describes closure
  list(set=set,get=get,setsolved=setsolved,getsolved=getsolved)
}


## Fuction uses closure to speedup large matrices inversion
##
cacheSolve <- function(x, ...) {
  slv <- x$getsolved()
  # Check if solution has been cached already
  if (!is.null(slv)) {
    ## Return  inverse matrix 'x' from cache
    return(slv)
  }
  # No solution cahced.
  mtx <-x$get()
  # Inverse data and cache in closure
  slv<-solve(mtx,...)
  x$setsolved(slv)
  # Return result.
  slv
}
