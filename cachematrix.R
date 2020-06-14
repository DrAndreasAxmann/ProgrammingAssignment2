## A pair of functions that caches the inverse of a matrix 
## to avoid computing the inverse repeatedly 

## makeCacheMatrix creates a special "matrix" object cachying its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) {
  x <<- y 
  m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
     setmatrix = setmatrix,
     getmatrix = getmatrix)
}

## cacheSolve computes the inverse of the matrix object returned by makeCacheMatrix
## If the inverse has already been computed (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache giving a corresponding message

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
