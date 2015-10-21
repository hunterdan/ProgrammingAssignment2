## The functions are to compute the inverse of a matrix the first time
## save the inverse in a free variable whose value is cached
## and simply take that value from the cache the second, third etc.
## time the function cacheSolve is called for that same matrix

## This function returns a list of functions associated with the 
## matrix argument: set assigns the matrix, get retrieves the matrix,
## setInverse sets the free variable inverse, and 
## getInverse retrieves the inverse matrix from the free variable

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## This function is used in conjunction with makeCacheMatrix and
## must be called with an object of type makeCacheMatrix(m) where
## m is the matrix to invert. If the  free variable inverse in 
## makeCacheMatrix has been assigned a value (use getInverse to see),
## that value is returned; otherwise the inverse
## is calculated and setInverse assigns the free variable inverse

cacheSolve <- function(x) {
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse<- solve(data)
  x$setInverse(inverse)
  inverse    ## Return a matrix that is the inverse of 'x'
}
