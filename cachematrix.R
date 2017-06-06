## pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(X = matrix()) {
  M <- NULL
  set <- function(Y) {
    X <<- Y
    M <<- NULL
  }
  get <- function() X
  setmatrix <- function(solve) M <<- solve
  getmatrix <- function() M
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(X, ...) {
  ## Return a matrix that is the inverse of 'x'
  M <- X["getmatrix"]
  if(!is.null(M)){
    message("getting cached data")
    return(M)
  }
  data <- X["get"]
  M <- solve(data,...)
  X["setmatrix(M)"]
  M
}

