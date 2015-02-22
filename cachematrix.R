## In order to calculate the inverse of a matrix two functions are defined: makeCacheMatrix & cacheSolve.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## Computing the inverse of a square matrix can be done with the solve() function in R
## makeCacheMatrix is actually a list containing a function to
##     set the matrix
##     get the matrix
##     set the inverse matrix
##     get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m< <-  solve
  getmatrix <- function() m
  list(set=set, get=get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated then cacheSolve gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)){    # check cache
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...) ## calculate inverse
  x$setmatrix(m)
  m
}
