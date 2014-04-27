## Put comments here that give an overall description of what your
## functions do

# Here a new matrix object is created that is able to store and restore 
# the calculated inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL      
  set <- function(y) {        
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


# This function gets the matrix object create by makeCacheMatrix and checks if the 
# inverse has already been calculated
# if this is the case it gets it from meory, otherwise calculates it and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
