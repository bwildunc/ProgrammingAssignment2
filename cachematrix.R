## The first function (makeCacheMatrix) creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) s <<- solve  ## uses solve to cache inverse of special matrix
  getmatrix <- function() s
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## The second function (cacheSolve) computes the inverse of the special matrix.
## This function first looks for a cache of the solution; if it has already been solved, the cached solution is provided.
## If not, a new inverse matrix is computed.

cacheSolve <- function(x, ...) {
  s <- x$getmatrix()  ## looks at the x matrix's cache
  if(!is.null(s)) {   ## if there is a cache (!null), returns the cached solution
    message("getting cached data")
    return(s)
  }
  data <- s$get()  ## if there is no cache, a new solution is computed
  s <- solve(data, ...)
  s$setmatrix(s)
  s               ## returns the result
}    
}