## write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  set_inv <- function(solve) invx <<- solve
  get_inv <- function() invx
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invx <- x$get_inv()
  if(!is.null(invx)) {
    message("getting cached data")
    return
  }
  data <- x$get()
  invx <- solve(data,...)
  x$set_inv(invx)
  invx
}
