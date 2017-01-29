## Put comments here that give an overall description of what your
## functions do

## this function returns a vector of functions
##gets a matrix
##sets a matrix
##gets a matrix inverse
##sets a matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x' 
## first check if inverse is cached,
## if cached return the cached version

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
