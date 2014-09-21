# The first function, makeCacheMatrix creates a special "matrix", which is really 
# a normal matrix containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse matrix
#4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# The following function calculates the inverse of the special "matrix" created 
# with the above function. However, it first checks to see if the inverse has 
# already been calculated. If so, it gets the inverse from the cache and skips 
# the computation. Otherwise, it calculates the inverse of the matrix and sets the 
# value of the inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
