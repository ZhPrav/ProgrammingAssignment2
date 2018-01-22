## Assignment 2 cource R Programming
## functions: 1. Creates a special matrix object with methods
##            2. Calculates the matrix inverse or get the cached matrix inverse

## Function makeCacheMatrix() creates a special matrix object with methods

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  init_x <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
    print("creating a new matrix with set")
  }
  get <- function() x
  setSolve <- function(solve) inv_m <<- solve
  getSolve <- function() inv_m
  list(set = set, 
       get = get,
       setSolve = setSolve,
       getSolve = getSolve 
       )
}

## Function cacheSolve() calculates the matrix inverse or get the cached matrix inverse
cacheSolve <- function(o, ...) {
  inv_m <- o$getSolve() 
  init_x <- o$get()
  ##print("printing init_x")
  ##print(init_x)
  if(!is.null(inv_m) )
    {
    message("getting cached data")
    return(inv_m)
  }
  data <- o$get()
  inv_m <- solve(data, ...)
  o$setSolve(inv_m)
  inv_m
}